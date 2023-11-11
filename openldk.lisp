(in-package :openldk)

(defvar *classes* (make-hash-table :test 'equal))
(defvar *cli-classpath* nil)
(defvar *verbose* nil)
(defvar *dump-dir* nil)
(defvar *context* nil)

(defvar *debug-codegen* nil)
(defvar *debug-unmuffle* nil)

(opts:define-opts
  (:name :verbose
   :description "produce verbose output"
   :short #\v
   :long "verbose")
  (:name :dump-dir
   :description "dump internal compiler debug output into this directory"
   :short #\d
   :arg-parser (lambda (arg) (setf *dump-dir* arg))
   :meta-var "DIRECTORY"
   :default nil
   :long "dump-dir")
  (:name :classpath
   :description "override CLASSPATH environment variable"
   :short #\c
   :arg-parser (lambda (arg) (setf *cli-classpath* arg))
   :meta-var "CLASSPATH"
   :default "."
   :long "classpath"))

(defun usage ()
  (opts:describe
   :prefix "openldk - copyright (C) 2023 Anthony Green <green@moxielogic.com>"
   :suffix "Distributed under the terms of MIT License"
   :usage-of "openldk"
   :args "classname"))

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun split-classpath (classpath)
  (split-sequence:split-sequence (uiop:inter-directory-separator) classpath))

(defun find-classpath-for-class (class classpath)
  (let* ((class (substitute (uiop:directory-separator-for-host) #\. class))
         (dir (find-if
               (lambda (path)
                 (let ((fqn (format nil "~A~A~A.class" path (uiop:directory-separator-for-host) class)))
                   (uiop:file-exists-p fqn)))
               (split-classpath classpath))))
    (if dir
        (uiop:file-exists-p (format nil "~A~A~A.class" dir (uiop:directory-separator-for-host) class))
        nil)))

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

(defun %clinit (class)
  (let ((class (gethash (slot-value class 'name) *classes*)))
    (labels ((clinit (class)
               (let ((super-class (gethash (slot-value class 'super) *classes*)))
                 (when super-class (clinit super-class)))
               (let ((<clinit>-method (find-if
                                       (lambda (method) (and (string= (slot-value method 'name) "<clinit>")
                                                             (string= (slot-value method 'descriptor) "()V")))
                                       (slot-value class 'methods))))
                 (when <clinit>-method
                   (%eval (list (intern (format nil "~A.<clinit>()V" (slot-value class 'name)) :openldk)))))))
      (clinit class))))

(defun insert-branch-targets (ssa-code branch-target-table)
  (let ((btt (make-hash-table)))
    (loop for insn in ssa-code
          for pc-index = (slot-value insn 'address)
          append (if (and (gethash pc-index branch-target-table)
                          (null (gethash pc-index btt)))
                     (progn
                       (setf (gethash pc-index btt) t)
                       (list (make-instance 'ssa-branch-target :address pc-index :index pc-index)
                             insn))
                     (list insn)))))

(defun make-ssa-try-catch (ssa-code start-pc end-pc)
  (let* ((before (remove-if (lambda (ssa-node)
                              (let ((pc (slot-value ssa-node 'pc-index)))
                                (>= pc start-pc)))
                            ssa-code))
         (after (remove-if (lambda (ssa-node)
                             (let ((pc (slot-value ssa-node 'pc-index)))
                               (< pc end-pc)))
                           ssa-code))
         (middle (list (remove-if (lambda (ssa-node)
                                    (let ((pc (slot-value ssa-node 'pc-index)))
                                      (or (< pc start-pc)
                                          (>= pc end-pc))))
                                  ssa-code))))
    (let ((aaa (append before middle after)))
      aaa)))

(defun insert-try-catch (ssa-code exception-table)
  (when exception-table
    (loop for i from 0 upto (1- (length exception-table))
          do (let ((ete (aref exception-table i)))
               (let ((start-pc (slot-value ete 'start-pc))
                     (end-pc (slot-value ete 'end-pc)))
                 (make-ssa-try-catch ssa-code start-pc end-pc)))))
  ssa-code)

; (apply #'%compile-method (restore "dumps/java/util/Vector<init>(II)V.compile-method"))

(defun %compile-method (class-name method-index)
  (let* ((class (gethash class-name *classes*))
         (method (aref (slot-value class 'methods) (1- method-index)))
         (exception-table (slot-value (gethash "Code" (slot-value method 'attributes)) 'exceptions))
         (code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
	 (max-locals (slot-value (gethash "Code" (slot-value method 'attributes)) 'max-locals))
         (length (length code))
         (*context* (make-instance '<context>
                                   :class class
                                   :classes *classes*
                                   :exception-table exception-table
                                   :bytecode code
                                   :is-clinit-p (string= "<clinit>"
                                                         (slot-value method 'name)))))
    (when *debug-codegen*
      (format t "; compiling ~A.~A~A~%" class-name (slot-value method 'name) (slot-value method 'descriptor))
      (force-output))
    (with-slots (pc fn-name) *context*
      (if (static-p method)
          (setf fn-name (format nil "~A.~A~A" (slot-value class 'name) (slot-value method 'name) (slot-value method 'descriptor)))
          (setf fn-name (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor))))
      (dump "compile-method" (list class-name method-index))
      (let* ((ssa-code-0
               (setf (slot-value *context* 'ssa-code)
                     (apply #'append
                            (loop
                              while (< pc length)
                              for result = (funcall
                                            (aref +opcodes+ (aref code pc))
                                            *context* code)
                              unless (null result)
                                collect result))))
             (blocks (build-basic-blocks ssa-code-0))
	     (lisp-code
	      (list (list 'block nil
			  (cons 'tagbody (loop for bloc in blocks append (codegen bloc))))))
             (definition-code
	      (let ((parameter-count (count-parameters (slot-value method 'descriptor))))
               (append (if (static-p method)
                           (list 'defun
                                 (intern fn-name :openldk)
                                 (loop for i from 1 upto parameter-count
                                       collect (intern (format nil "arg~A" (1- i)) :openldk)))
                           (list 'defmethod
                                 (intern fn-name :openldk)
                                 (cons (list (intern "this" :openldk) (intern (slot-value class 'name) :openldk))
                                       (loop for i from 1 upto parameter-count
                                             collect (intern (format nil "arg~A" i) :openldk)))))
		       (list (list 'format 't "tracing: ~A.~A~%" class-name fn-name))
                       (if (slot-value *context* 'uses-stack-p)
                           (list (append (list 'let (append (list (list 'stack (list 'list)))
                                                            (if (static-p method)
                                                                (append (loop for i from 1 upto parameter-count
									      collect (list (intern (format nil "local-~A" (1- i)) :openldk) (intern (format nil "arg~A" (1- i)) :openldk)))
									(loop for i from (1+ parameter-count) upto max-locals
									      collect (list (intern (format nil "local-~A" (1- i)) :openldk))))
                                                                (append (cons (list (intern "local-0" :openldk) (intern "this" :openldk))
									      (loop for i from 1 upto parameter-count
										    collect (list (intern (format nil "local-~A" i) :openldk) (intern (format nil "arg~A" i) :openldk))))
									(loop for i from (+ 2 parameter-count) upto max-locals
									      collect (list (intern (format nil "local-~A" (1- i)) :openldk)))))))
                                         lisp-code))
                           lisp-code)))))
        (%eval definition-code)))))

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
    (t nil)))

(defun emit-<class> (class)
  (let ((defclass-code (with-slots (name super fields) class
                         (list
                          'progn
                          (list
                           'defclass (intern name :openldk)
                           (if super (list (intern super :openldk)) (list))
                           (map 'list
                                (lambda (f)
				  (format t "FIELD ~A: ~A~%" (slot-value f 'name) (slot-value f 'descriptor))
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
          (let ((method-index 0))
            (with-slots (name super methods) class
              (remove nil (map 'list
                               (lambda (m)
                                 (if (native-p m)
                                     (progn
                                       (format t "NATIVE METHOD: ~A~%" (slot-value m 'name))
                                       (incf method-index)
                                       nil)
                                     (if (static-p m)
                                         (list 'defun (intern (format nil "~A.~A~A" (slot-value class 'name) (slot-value m 'name) (slot-value m 'descriptor)) :openldk)
                                               (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                     collect (intern (format nil "arg~A" i) :openldk))
                                               (list '%compile-method (slot-value class 'name) (incf method-index))
                                               (cons (intern (format nil "~A.~A~A" (slot-value class 'name) (slot-value m 'name) (slot-value m 'descriptor)) :openldk)
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk))))
                                         (list 'defmethod (intern (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)) :openldk)
                                               (cons (list (intern "this" :openldk) (intern (slot-value (slot-value m 'class) 'name) :openldk))
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk)))
                                               (list '%compile-method (slot-value class 'name) (incf method-index))
                                               (cons (intern (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)) :openldk)
                                                     (cons (intern "this" :openldk)
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk))))))))
                               methods))))))
    (append defclass-code methods-code)))

(defvar *condition-table* (make-hash-table))

(defun classload (classname classpath)
  (let ((class (gethash classname *classes*)))
    (if class
        class
        (let ((fqfn (find-classpath-for-class classname classpath)))
          (if fqfn
              (let* ((class
                       (let ((c (read-classfile fqfn)))
                         (setf (gethash classname *classes*) c)
                         c))
                     (super (let ((super (slot-value class 'super)))
                              (when super (classload super classpath)))))
                (let ((code (emit-<class> class)))
                  (%eval code))
                (when (and (not (string= classname "java/lang/Throwable"))
                           (subtypep (find-class (intern classname :openldk)) (find-class '|java/lang/Throwable|)))
                  (let ((condition-symbol (intern (format nil "condition-~A" classname) :openldk)))
                    (setf (gethash (find-class (intern classname :openldk)) *condition-table*) condition-symbol)
                    (let ((ccode (list 'define-condition condition-symbol
                                       (list (intern (format nil "condition-~A" (slot-value super 'name)) :openldk)) (list))))
                    (%eval ccode))))
                class)
              (format t "ERROR: Can't find ~A on classpath ~A~%" classname CLASSPATH))))))

(defun main ()
  (let ((CLASSPATH (uiop:getenv "CLASSPATH"))
        (LDK_DEBUG (uiop:getenv "LDK_DEBUG")))

    (when LDK_DEBUG
      (progn
        (when (find #\c LDK_DEBUG)
          (setf *debug-codegen* t))
        (when (find #\u LDK_DEBUG)
          (setf *debug-unmuffle* t))))

    (multiple-value-bind (options free-args)
        (handler-case
            (handler-bind ((opts:unknown-option #'unknown-option))
              (opts:get-opts))
          (opts:missing-arg (condition)
            (format t "fatal: option ~s needs an argument!~%"
                    (opts:option condition)))
          (opts:arg-parser-failed (condition)
            (format t "fatal: cannot parse ~s as argument of ~s~%"
                    (opts:raw-arg condition)
                    (opts:option condition))))

      (when-option (options :verbose)
                   (setf *verbose* t))

      (if (not (eq 1 (length free-args)))
          (usage)
          (progn
            (%clinit (classload "java/lang/Object" ".:jre8/"))
            (%clinit (classload "java/lang/String" ".:jre8/"))
            (%clinit (classload "java/lang/ClassLoader" ".:jre8/"))
            (%clinit (classload "java/lang/Class" ".:jre8/"))
            (let* ((class (classload (car free-args) CLASSPATH)))
              (%clinit class)
              (%eval (list (intern (format nil "~A.main([Ljava/lang/String;)V" (slot-value class 'name)) :openldk) #()))))))))
