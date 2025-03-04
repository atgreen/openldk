;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2023, 2024, 2025  Anthony Green <green@moxielogic.com>
;;;
;;; This file is part of OpenLDK.

;;; OpenLDK is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.

;;; OpenLDK is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with OpenLDK; see the file COPYING.  If not, please see
;;; <http://www.gnu.org/licenses/>.

;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library.  Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.

;;; As a special exception, the copyright holders of this library give
;;; you permission to link this library with independent modules to
;;; produce an executable, regardless of the license terms of these
;;; independent modules, and to copy and distribute the resulting
;;; executable under terms of your choice, provided that you also
;;; meet, for each linked independent module, the terms and conditions
;;; of the license of that module.  An independent module is a module
;;; which is not derived from or based on this library.  If you modify
;;; this library, you may extend this exception to your version of the
;;; library, but you are not obligated to do so.  If you do not wish
;;; to do so, delete this exception statement from your version.

(annot:enable-annot-syntax)

(in-package :openldk)

;; This is a hack to make sure getCallerClass works.
;; It would be good if we didn't have to do this.
(defvar *force-this-to-be-used* nil)

(defun %eval (code)
  (when *debug-codegen*
    (pprint code)
    (format t "~%"))
  (if *debug-unmuffle*
      (eval code)
      (handler-bind
          (#+ansi-cl
           (style-warning (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'muffle-warning))))
        (eval code))))

(defun lispize-method-name (name)
  (subseq name 0 (1+ (position #\) name))))

(defun make-exception-handler-table (context)
  (let ((exception-table (exception-table context))
        (exception-handler-table (make-hash-table)))
    (when exception-table
      (loop for i from 0 below (length exception-table)
            for ete = (aref exception-table i)
            do (setf (gethash (handler-pc ete) exception-handler-table) t)))
    exception-handler-table))

(defun fix-stack-variables (stack-vars)

  (let ((groups (make-hash-table :test 'equal)))
    ;; Step 1: Group stack-variables by their var-numbers
    (dolist (stack-var stack-vars)
      (let ((var-numbers (slot-value stack-var 'var-numbers)))
        (dolist (num var-numbers)
          (push stack-var (gethash num groups)))))

    ;; Step 2: Merge groups that share common numbers
    (let ((merged-groups ()))
      (maphash (lambda (num stack-vars)
                 (declare (ignore num))
                 (let ((merged-group ()))
                   (dolist (stack-var stack-vars)
                     (unless (member stack-var merged-group :test #'eq)
                       (push stack-var merged-group)))
                   (push merged-group merged-groups)))
               groups)

      ;; Step 3: Update var-numbers for each stack-variable
      (dolist (group merged-groups)
        (let ((all-var-numbers ()))
          (dolist (stack-var group)
            (setf all-var-numbers (union all-var-numbers (slot-value stack-var 'var-numbers))))
          (dolist (stack-var group)
            (setf (slot-value stack-var 'var-numbers) all-var-numbers))))))

  stack-vars)

(defun propagate-copies (ir-code single-assignment-table)
  ;; Scan through the code, looking for assignments to stack variables
  ;; with single assignments.  Remove those assignments.
  (mapcar (lambda (insn)
            (if (and (typep insn 'ir-assign)
                     (let ((lvalue (slot-value insn 'lvalue))
                           (rvalue (slot-value insn 'rvalue)))
                       (if (and (typep lvalue '<stack-variable>)
                                (eq (length (slot-value lvalue 'var-numbers)) 1)
                                (not (side-effect-p rvalue)))
                           (setf (gethash lvalue single-assignment-table) (slot-value insn 'rvalue))
                           nil)))
                (make-instance 'ir-nop :address (address insn))
                insn))
          ir-code))

(defun initialize-arrays (ir-code)
  (let ((code-array (coerce ir-code 'vector)))
    (loop for i below (length ir-code)
          for insn = (aref code-array i)
          when (and (typep insn 'ir-assign)
                    (let ((rvalue (slot-value insn 'rvalue)))
                      (and (typep rvalue 'ir-new-array)
                           (typep (size rvalue) '<stack-variable-constant-int>))))
            do (let* ((rvalue (slot-value insn 'rvalue))
                      (init-element
                        (case (atype rvalue)
                          ;; Determine the initial element based on the array type
                          (4 0)        ; Integer
                          (5 #\Null)   ; Character
                          (6 0.0)      ; Single-precision float
                          (7 0.0d0)    ; Double-precision float
                          ((8 9 10 11) 0) ; Other integer types (assuming default to 0)
                          (t nil))))   ; Default to nil for unknown types
                 (let* ((pc (1+ i))
                        (values (loop for array-index from 0 below (slot-value (size rvalue) 'value)
                                      collect (progn
                                                (loop until (not (typep (aref code-array pc) 'ir-nop))
                                                      do (incf pc))
                                                (let ((insn (aref code-array pc)))
                                                  (incf pc)
                                                  (if (typep insn 'ir-xastore)
                                                      (code (codegen (value insn) *context*))
                                                      (return nil))))))
                        (array (if (zerop (slot-value (size rvalue) 'value))
                                   #()
                                   (if values
                                       (progn
                                         (loop for nop-pc from (1+ i) below pc
                                               do (setf (aref code-array nop-pc) (make-instance 'ir-nop :address (address (aref code-array nop-pc)))))
                                         values)
                                       (make-array (slot-value (size rvalue) 'value)
                                                   :initial-element init-element)))))
                   (setf (slot-value insn 'rvalue)
                         (make-instance 'ir-array-literal
                                        :address (address insn)
                                        :value array)))
                 (assert (typep (aref code-array (1- i)) 'ir-nop))))
    (coerce code-array 'list)))

(defun %compile-method (class-name method-index)
  (let* ((class (%get-ldk-class-by-bin-name class-name))
         (method (aref (slot-value class 'methods) (1- method-index))))
    (when (gethash "Code" (slot-value method 'attributes)) ; otherwise it is abstract
      (let* ((parameter-hints (gen-parameter-hints (descriptor method)))
             (exception-table (slot-value (gethash "Code" (slot-value method 'attributes)) 'exceptions))
             (code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
             (max-locals (slot-value (gethash "Code" (slot-value method 'attributes)) 'max-locals))
             (length (length code))
             (*context* (make-instance '<context>
                                       :class class
                                       :classes *ldk-classes-by-bin-name*
                                       :exception-table exception-table
                                       :bytecode code
                                       :insn-size (make-array (length code) :element-type 'fixnum :initial-element -1)
                                       :next-insn-list (make-array (length code) :initial-element nil)
                                       :stack-state-table (make-hash-table)
                                       :pc 0
                                       :is-clinit-p (string= "<clinit>" (slot-value method 'name)))))
        (setf (svcount *context*) 0)
        (when *debug-codegen*
          (format t "; compiling ~A.~A~%" class-name (lispize-method-name (format nil "~A~A" (name method) (descriptor method))))
          (force-output))
        (if (static-p method)
            (setf (fn-name *context*) (format nil "~A.~A" (slot-value class 'name) (lispize-method-name (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor)))))
            (setf (fn-name *context*) (format nil "~A" (lispize-method-name (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor))))))
        (let* ((exception-handler-table (make-exception-handler-table *context*))
               (ir-code-0
                 (setf (ir-code *context*)
                       (let ((code (apply #'append
                                          (loop
                                            while (and (< (pc *context*) length))
                                            for no-record-stack-state? = (find (aref +opcodes+ (aref code (pc *context*))) '(:GOTO))
                                            for result = (progn
                                                           (let ((stk (gethash (pc *context*) (stack-state-table *context*))))
                                                             (when stk
                                                               (setf (stack *context*) (car stk))))
                                                           (when *debug-bytecode*
                                                             (format t "~&; c[~A] ~A ~@<~A~:@>" (pc *context*) (aref +opcodes+ (aref code (pc *context*))) (stack *context*)))
                                                           (let* ((pc-start (pc *context*)))
                                                             (if (gethash pc-start exception-handler-table)
                                                                 (let ((var (make-stack-variable *context* pc-start :REFERENCE)))
                                                                   (push var (stack *context*))
                                                                   (cons (make-instance 'ir-assign
                                                                                        :address pc-start
                                                                                        :lvalue var
                                                                                        :rvalue (make-instance 'ir-condition-exception))
                                                                         (mapcar (lambda (insn)
                                                                                   (with-slots (address) insn
                                                                                     (setf address (+ address 0.1)))
                                                                                   insn)
                                                                                 (funcall
                                                                                  (aref +opcodes+ (aref code (pc *context*)))
                                                                                  *context* code))))
                                                                 (funcall
                                                                  (aref +opcodes+ (aref code (pc *context*)))
                                                                  *context* code))))
                                            unless no-record-stack-state?
                                              do (%record-stack-state (pc *context*) *context*)
                                            unless (null result)
                                              collect result))))
                         ;; Do stack analysis to merge stack variables
                         (maphash (lambda (k v)
                                    (when (> (length v) 1)
                                      (reduce #'merge-stacks v)))
                                  (stack-state-table *context*))
                         (fix-stack-variables (stack-variables *context*))
                         (initialize-arrays (propagate-copies code (single-assignment-table *context*))))))
                         ; (propagate-copies code (single-assignment-table *context*)))))
               ;; (sdfdfd (print ir-code-0))
               (blocks (build-basic-blocks ir-code-0))
               (lisp-code
                 (list (list 'block nil
                             (append (list 'tagbody)
                                     (mapcan (lambda (x) (if (listp x) x (list x)))
                                             (loop for block in blocks
                                                   for code = (codegen-block block block)
                                                   when code
                                                     collect (progn
                                                               code)))))))
               (traced-lisp-code (if *debug-trace* `((unwind-protect
                                                          ,(car lisp-code)
                                                       (incf *call-nesting-level* -1)))
                                     lisp-code))

               (definition-code
                 (let ((parameter-count (count-parameters (slot-value method 'descriptor))))
                   (let ((args (if (static-p method)
                                   (loop for i from 1 upto parameter-count
                                         collect (intern (format nil "arg~A" (1- i)) :openldk))
                                   (loop for i from 1 upto parameter-count
                                         collect (intern (format nil "arg~A" i) :openldk)))))
                     `(progn
                        ,(append (if (static-p method)
                                    (list 'defun (intern (fn-name *context*) :openldk) args)
                                    (list 'defmethod
                                          (intern (fn-name *context*) :openldk)
                                          (cons (list (intern "this" :openldk) (intern (slot-value class 'name) :openldk))
                                                args)))
                                (when *debug-trace*
                                  (list (list 'format 't "~&~V@A trace: entering ~A.~A(~{~A~^ ~}) ~A~%"
                                              (list 'incf '*call-nesting-level* 1) "*"
                                              class-name (fn-name *context*) (if *debug-trace-args*
                                                                                 (cons 'list args)
                                                                                 ())
                                              (if (not (static-p method)) (intern "this" :openldk) ""))))
                                (when (not (static-p method))
                                  (list (list 'setf '*force-this-to-be-used* (intern "this" :openldk))
                                        ))
;;                                        (list 'describe (intern "this" :openldk))))
                                (let ((i 0)
                                      (pc -1))
                                  (list (format nil "bridge=~A" (bridge-p method))
                                        (append (list 'let (if (static-p method)
                                                               (append (list (list '|condition-cache|))
                                                                       (remove-duplicates
                                                                        (loop for var in (stack-variables *context*)
                                                                              unless (gethash var (single-assignment-table *context*))
                                                                                collect (list (intern (format nil "s{~{~A~^,~}}" (sort (copy-list (var-numbers var)) #'<)) :openldk)))
                                                                        :test #'equal)
                                                                       (loop for ph in parameter-hints
                                                                             collect (list (intern (format nil "local-~A" i) :openldk) (intern (format nil "arg~A" (incf pc)) :openldk))
                                                                             do (if (eq ph t) (incf i) (incf i 2)))
                                                                       (loop for pc from (- parameter-count 2) upto max-locals
                                                                             collect (list (intern (format nil "local-~A" (1- (incf i))) :openldk))))
                                                               (append (list (list '|condition-cache|))
                                                                       (remove-duplicates
                                                                        (loop for var in (stack-variables *context*)
                                                                              unless (gethash var (single-assignment-table *context*))
                                                                                collect (list (intern (format nil "s{~{~A~^,~}}" (sort (copy-list (var-numbers var)) #'<)) :openldk)))
                                                                        :test #'equal)
                                                                       (append (list (list (intern "local-0" :openldk) (intern "this" :openldk)))
                                                                               (loop for ph in parameter-hints
                                                                                     collect (list (intern (format nil "local-~A" (1+ i)) :openldk) (intern (format nil "arg~A" (1+ (incf pc))) :openldk))
                                                                                     do (if (eq ph t) (incf i) (incf i 2)))
                                                                               (loop for x from parameter-count upto (1+ max-locals)
                                                                                     collect (list (intern (format nil "local-~A" (incf i)) :openldk)))))))
                                                traced-lisp-code)))))))))
          (%eval definition-code))))))

(defun %clinit (class)
  (let ((class (gethash (name class) *ldk-classes-by-bin-name*)))
    (assert
     (or class (error "Can't find ~A" class)))
    (labels ((clinit (class)
               (let ((super-class (gethash (slot-value class 'super) *ldk-classes-by-bin-name*)))
                 (when (and super-class
                            (not (initialized-p super-class)))
                   (clinit super-class)))
               (let ((<clinit>-method (find-if
                                       (lambda (method) (and (string= (slot-value method 'name) "<clinit>")
                                                             (string= (slot-value method 'descriptor) "()V")))
                                       (slot-value class 'methods))))
                 (when <clinit>-method
                   (setf (initialized-p class) t)
                   (%eval (list (intern (format nil "~A.<clinit>()" (slot-value class 'name)) :openldk)))))))
      (clinit class))))

(defun open-java-classfile-on-classpath (class)
  (let* ((class (substitute (uiop:directory-separator-for-host) #\. class)))
    (loop for cpe in *classpath*
          for classfile-stream = (open-java-classfile cpe class)
          when classfile-stream
            return classfile-stream)))

(defun initform-from-descriptor (descriptor)
  (cond
    ((string= descriptor "I")
     0)
    ((string= descriptor "J")
     0)
    ((string= descriptor "F")
     0.0)
    ((string= descriptor "D")
     0.0d0)
    ((string= descriptor "S")
     0)
    ((string= descriptor "B")
     0)
    ((string= descriptor "C")
     #\0)
    ((string= descriptor "Z")
     0)
    (t nil)))

(defun emit-<class> (class)
  (let ((defclass-code (with-slots (name super interfaces fields) class
                         (list
                          'progn
                          (list
                           'defclass (intern name :openldk)
                           (if (or super interfaces) (append (if super (list (intern super :openldk)) nil)
                                                             (let ((ifaces (mapcar (lambda (i) (intern i :openldk))
                                                                                   (coerce interfaces  'list))))
                                                               (sort (copy-list ifaces) #'subtypep)))
                               (list))
                           (map 'list
                                (lambda (f)
                                  (list (intern (slot-value f 'name) :openldk)
                                        :initform (initform-from-descriptor (slot-value f 'descriptor))
                                        :allocation
                                        (if (eq 0 (logand 8 (slot-value f 'access-flags))) :instance :class)))
                                fields))
                          (list
                           'defparameter (intern (format nil "+static-~A+" (intern name)) :openldk)
                           (list
                            'make-instance (list 'quote (intern name :openldk)))))))
        (methods-code
          (let ((method-index 0)
                (done-method-table (make-hash-table :test #'equal)))
            (if (find (name class) '("java/util/jar/JarInputStream" "java/util/zip/ZipFile" "java/util/zip/ZipInputStream") :test #'equal)
                nil
                (with-slots (name super methods) class
                  (remove nil (map 'list
                                   (lambda (m)
                                     (if (or (native-p m) (null (gethash "Code" (attributes m)))
                                             (and (bridge-p m) (gethash (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) done-method-table)))
                                         (progn
                                           (incf method-index)
                                           nil)
                                         (progn
                                           (setf (gethash (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) done-method-table) t)
                                           (if (static-p m)
                                               (list 'defun (intern (format nil "~A.~A" (slot-value class 'name) (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))) :openldk)
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk))
                                                     (list '%compile-method (slot-value class 'name) (incf method-index))
                                                     (cons (intern (format nil "~A.~A" (slot-value class 'name) (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))) :openldk)
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk))))
                                               (list 'defmethod (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk)
                                                     (cons (list (intern "this" :openldk) (intern (slot-value (slot-value m 'class) 'name) :openldk))
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk)))
                                                     (list '%compile-method (slot-value class 'name) (incf method-index))
                                                     (cons (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk)
                                                           (cons (intern "this" :openldk)
                                                                 (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                       collect (intern (format nil "arg~A" i) :openldk)))))))))
                                   methods)))))))

    (append defclass-code methods-code)))

(defun classload (classname)
  (let ((classname (coerce classname 'string)))
    (assert (not (find #\. classname)))
    (assert (> (length classname) 0))
    (let ((class (gethash classname *ldk-classes-by-bin-name*))
          (classname-symbol (intern classname :openldk)))
      (if class
          class
          (let ((classfile-stream (open-java-classfile-on-classpath classname)))
            (when *debug-load*
              (format t "~&; LOADING ~A~%" classname))
            (if classfile-stream
                (unwind-protect

                     (let* ((class
                              (let ((c (read-classfile classfile-stream)))
                                (setf (gethash classname *ldk-classes-by-bin-name*) c)
                                (setf (gethash (substitute #\. #\/ classname) *ldk-classes-by-fq-name*) c)
                                c))
                            (super (let ((super (slot-value class 'super)))
                                     (when super (classload super))))
                            (interfaces (let ((interfaces (slot-value class 'interfaces)))
                                          (when interfaces
                                            (mapcar (lambda (i) (classload i)) (coerce interfaces 'list))))))
                       (let ((klass (or (%get-java-class-by-bin-name classname t)
                                        (let ((klass (make-instance '|java/lang/Class|))
                                              (cname (jstring (substitute #\. #\/ classname)))
                                              (cloader nil)) ;; (make-instance '|java/lang/ClassLoader|)))
                                          (with-slots (|name| |classLoader|) klass
                                            (setf |name| cname)
                                            (setf |classLoader| cloader))
                                          klass))))
                         (setf (java-class class) klass)
                         (setf (gethash classname *java-classes-by-bin-name*) klass)
                         (setf (gethash (substitute #\. #\/ classname) *java-classes-by-fq-name*) klass))

                       (let ((code (emit-<class> class)))
                         (%eval code))

                       (dolist (ic (gethash "InnerClasses" (attributes class)))
                         (when (zerop (outer-class-info-index ic))
                           (let* ((class-reference (aref (constant-pool class) (inner-class-info-index ic)))
                                  (class-name (aref (constant-pool class) (index class-reference))))
                             (push class-name (inner-classes class)))))

                       ;; Emit the class initializer
                       (let ((lisp-class (find-class (intern (substitute #\/ #\. classname) :openldk))))
                         (closer-mop:finalize-inheritance lisp-class)
                         (let ((icc (append (list 'defun (intern (format nil "%clinit-~A" (substitute #\/ #\. classname)) :openldk) (list))
                                            ;; (list (list 'format 't ">>> clinit ~A~%" lisp-class))
                                            (loop for k in (reverse (closer-mop:class-precedence-list lisp-class))
                                                  for clinit-function = (intern (format nil "~a.<clinit>()" (class-name k)) :openldk)
                                                  when (fboundp clinit-function)
                                                    collect (let ((ldkclass (gethash (format nil "~A" (class-name k)) *ldk-classes-by-bin-name*)))
                                                              (list 'unless (list 'initialized-p ldkclass)
                                                                    (list 'setf (list 'initialized-p ldkclass) t)
                                                                    (list clinit-function)))))))
                           (%eval icc)))

                       (when (and (not (string= classname "java/lang/Throwable"))
                                  (subtypep classname-symbol (find-class '|java/lang/Throwable|)))
                         (let ((condition-symbol (intern (format nil "condition-~A" classname) :openldk)))
                           (setf (gethash (find-class (intern classname :openldk)) *condition-table*) condition-symbol)
                           (let ((ccode `(define-condition ,condition-symbol ( ,(intern (format nil "condition-~A" (slot-value super 'name)) :openldk))
                                           ())))
                             (%eval ccode))
                           (let ((ccode `(defmethod %lisp-condition ((throwable ,(intern (format nil "~A" classname) :openldk)))
                                           (let ((c (make-condition (quote ,(intern (format nil "condition-~A" classname) :openldk)))))
                                             (setf (slot-value c '|objref|) throwable)
                                             c))))
                             (%eval ccode))))

                       ;; Load all of the field classes
                       (loop for field across (fields class)
                             do (classload (slot-value (slot-value field 'class) 'name)))

                       class)
                  (close classfile-stream))
                nil))))))

(defun ensure-JAVA_HOME ()
  (let ((JAVA_HOME (uiop:getenv "JAVA_HOME")))
    (unless JAVA_HOME
      (format *error-output* "~%OpenLDK Error: JAVA_HOME environment variable not set~%")
      (uiop:quit 1))

    (unless (uiop:file-exists-p (concatenate 'string JAVA_HOME "/lib/rt.jar"))
      (format *error-output* "~%OpenLDK Error: Cannot find $JAVA_HOME/lib/rt.jar~%")
      (uiop:quit 1))))

@cli:command
(defun main (mainclass &optional (args (list)) &key dump-dir classpath)
  (declare
   (cli:parser (list identity) args)
   (cli:parser identity classpath)
   (cli:parser identity dump-dir))
  "openldk - copyright (C) 2023-2024 Anthony Green <green@moxielogic.com>
   Distributed under the terms of the GPLv3 + Classpath Exception

   MAINCLASS: The class with the static main method to execute.

   ARGS: Java program command line arguments

   CLASSPATH: The classpath from which classes are loaded.

   DUMP-DIR: The directory into which internal debug info is dumped."

  (ensure-JAVA_HOME)

  ;; If classpath isn't set on the command line, then get it
  ;; from the LDK_CLASSPATH environment variable.
  (unless classpath
    (setf classpath
          (concatenate 'string
                       (uiop:getenv "LDK_CLASSPATH")
                       ":"
                       (format nil "~{~A~^:~}"
                               (mapcar #'namestring
                                       (directory
                                        (concatenate 'string
                                                     (uiop:getenv "JAVA_HOME")
                                                     "/lib/*.jar")))))))

  (let ((LDK_DEBUG (uiop:getenv "LDK_DEBUG")))
    (when LDK_DEBUG
      (progn

        (when (find #\c LDK_DEBUG)
          (setf *debug-codegen* t))
        (when (find #\l LDK_DEBUG)
          (setf *debug-load* t))
        (when (find #\s LDK_DEBUG)
          (setf *debug-slynk* t))
        (when (find #\t LDK_DEBUG)
          (setf *debug-trace* t))
        (when (find #\T LDK_DEBUG)
          (setf *debug-trace-args* t))
        (when (find #\b LDK_DEBUG)
          (setf *debug-bytecode* t))
        (when (find #\x LDK_DEBUG)
          (setf *debug-x* t))
        (when (find #\u LDK_DEBUG)
          (setf *debug-unmuffle* t)))))

  (when *debug-slynk*
    (slynk:create-server :port 2025)
    (sleep 10))

  (setf *dump-dir* dump-dir)

  (setf *classpath*
        (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
              collect (if (str:ends-with? ".jar" cpe)
                          (make-instance 'jar-classpath-entry :jarfile cpe)
                          (make-instance 'dir-classpath-entry :dir cpe))))

  (let* ((class (classload (substitute #\/ #\. mainclass)))
         (argv (make-array (length args))))
    (assert (or class (error "Can't load ~A" mainclass)))
    (dotimes (i (length args))
      (let ((arg (jstring (nth i args))))
        (setf (aref argv i) arg)))
    (%clinit class)

    ;; The `main` method may be in a superclass of CLASS.  Search for it.
    (labels ((find-main (class)
               (let ((main-symbol (intern (format nil "~A.main([Ljava/lang/String;)" (name class)) :openldk)))
                 (if (fboundp main-symbol)
                     main-symbol
                     (find-main (gethash (super class) *ldk-classes-by-bin-name*))))))
      (let ((main-symbol (find-main class)))
        (if main-symbol
            (%eval (list main-symbol argv))
            (error "Main method not found in class ~A." (name class)))))))

(defun main-wrapper ()
  "Main entry point into OpenLDK. Process command line errors here."
  (let ((backtrace nil))
    (handler-bind ((error
                     (lambda (condition)
                       (format *error-output* "~&*** Caught ERROR: ~A~%" condition)
                       #+sbcl (sb-debug:backtrace)
                       (uiop:quit 1))))
      (main-command))))

(defun make-image ()

  (ensure-JAVA_HOME)

  (let ((classpath
          (concatenate 'string
                       (uiop:getenv "LDK_CLASSPATH")
                       ":"
                       (format nil "~{~A~^:~}"
                               (mapcar #'namestring
                                       (directory
                                        (concatenate 'string
                                                     (uiop:getenv "JAVA_HOME")
                                                     "/lib/*.jar")))))))

    (setf *classpath*
          (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
                collect (if (str:ends-with? ".jar" cpe)
                            (make-instance 'jar-classpath-entry :jarfile cpe)
                            (make-instance 'dir-classpath-entry :dir cpe)))))

  (setf *debug-load* t)

  ;; We need to hand load these before Class.forName0 will work.
  (%clinit (classload "java/lang/Object"))
  (%clinit (classload "java/lang/String"))
  (%clinit (classload "java/lang/Class"))
  (%clinit (classload "java/lang/ClassLoader"))

  (handler-case

      (let ((boot-class-loader (make-instance '|java/lang/ClassLoader|)))

        (dolist (p '(("byte" . "B") ("char" . "C") ("int" . "I")
                     ("short" . "S") ("long" . "J") ("double" . "D")
                     ("float" . "F") ("boolean" . "Z") ("void" . "Z")))
          (let ((class (make-instance '|java/lang/Class|)))
            (setf (slot-value class '|name|) (ijstring (car p)))
            (setf (gethash (car p) *java-classes-by-fq-name*) class)))
;            (setf (gethash (cdr p) *java-classes-by-bin-name*) class)))

        ;; Preload some important classes.
        (dolist (c '("java/lang/Boolean"
                     "java/lang/Character"
                     "java/lang/Byte"
                     "java/lang/Short"
                     "java/lang/Integer"
                     "java/lang/Long"
                     "java/lang/Float"
                     "java/lang/Double"
                     "java/nio/LongBuffer"
                     "java/lang/Void"
                     "java/lang/ClassLoader"
                     "java/security/PrivilegedAction"
                     "java/lang/StackTraceElement"
                     "java/lang/System"
                     "java/lang/ThreadGroup"
                     "java/lang/Thread"
                     "java/lang/ref/SoftReference"
                     "java/util/Properties"))
          (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil boot-class-loader nil))

        (let ((props (make-instance '|java/util/Properties|)))
          (|<init>()| props)
          (setf (slot-value |+static-java/lang/System+| '|props|) props))

        (|java/lang/System.initializeSystemClass()|)

        (|<init>()| boot-class-loader)

        (dolist (c '("java/lang/invoke/MethodHandles"
                     "java/lang/invoke/MethodHandles$Lookup"))
          (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil boot-class-loader nil)))

    (|condition-java/lang/Throwable| (c)
      (format t "~&Exception: ~A~%" c)
      (let ((cause (slot-value (slot-value c '|objref|) '|cause|)))
      (format t "   Caused by: ~A~%" cause)
      ;; (|printStackTrace()| (slot-value c '|objref|))
      (format t "~&~A~%" (slot-value cause '|backtrace|)))))

  (setf *debug-load* nil)

  (sb-ext:save-lisp-and-die "openldk" :executable t :save-runtime-options t :toplevel #'main-wrapper))
