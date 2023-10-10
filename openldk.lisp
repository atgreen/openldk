(in-package :openldk)

(defvar *classes* (make-hash-table :test 'equal))

(defvar *debug-codegen* nil)
(defvar *debug-unmuffle* nil)

(opts:define-opts
  (:name :verbose
   :description "produce verbose output"
   :short #\v
   :long "verbose")
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
  (maphash (lambda (key value)
             (format t "~A -> ~A~%" key value))
           branch-target-table)
  (let ((btt (make-hash-table)))
    (loop for insn in ssa-code
          for pc-index = (slot-value insn 'pc-index)
          do (format t "[~A][~A] ~A~%" pc-index (gethash pc-index branch-target-table) insn))
    (loop for insn in ssa-code
          for pc-index = (slot-value insn 'pc-index)
          append (if (and (gethash pc-index branch-target-table)
                          (null (gethash pc-index btt)))
                     (progn
                       (setf (gethash pc-index btt) t)
                       (list (make-instance 'ssa-branch-target :index pc-index)
                             insn))
                     (list insn)))))

(defun %compile-method (class-name method-index)
  (let* ((class (gethash class-name *classes*))
         (method (aref (slot-value class 'methods) (1- method-index))))
    (let* ((code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
           (length (length code))
           (context (make-instance '<context>
                                   :class class
                                   :is-clinit-p (string= "<clinit>"
                                                         (slot-value method 'name)))))
      (when *debug-codegen*
        (format t "; compiling ~A.~A~A~%" class-name (slot-value method 'name) (slot-value method 'descriptor))
        (force-output))
      (with-slots (pc fn-name) context
        (if (static-p method)
            (setf fn-name (format nil "~A.~A~A" (slot-value class 'name) (slot-value method 'name) (slot-value method 'descriptor)))
            (setf fn-name (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor))))
        (let* ((ssa-code-pre-branch-targets
                 (apply #'append
                        (loop
                          while (< pc length)
                          for result = (funcall (aref +opcodes+ (aref code pc)) context code)
                          unless (null result)
                            collect result)))
               (ssa-code (insert-branch-targets ssa-code-pre-branch-targets
                                                (find-branch-targets code)))
               (lisp-code (mapcar (lambda (ssa-node)
                                    (codegen ssa-node))
                                  ssa-code))
               (code (append (if (static-p method)
                                 (list 'defun
                                       (intern fn-name :openldk)
                                       (loop for i from 1 upto (count-parameters (slot-value method 'descriptor))
                                             collect (intern (format nil "arg~A" i) :openldk)))
                                 (list 'defmethod
                                       (intern fn-name :openldk)
                                       (cons (list (intern "this" :openldk) (intern (slot-value class 'name) :openldk))
                                             (loop for i from 1 upto (count-parameters (slot-value method 'descriptor))
                                                   collect (intern (format nil "arg~A" i) :openldk)))))
                             (list (list 'let (append (remove nil
                                                              (if (static-p method)
                                                                  (loop for i from 1 upto (count-parameters (slot-value method 'descriptor))
                                                                        collect (list (intern (format nil "local-~A" (1- i)) :openldk) (intern (format nil "arg~A" i) :openldk)))
                                                                  (cons (list (intern "local-0" :openldk) (intern "this" :openldk))
                                                                        (loop for i from 1 upto (count-parameters (slot-value method 'descriptor))
                                                                              collect (list (intern (format nil "local-~A" i) :openldk) (intern (format nil "arg~A" i) :openldk)))))))
                                         (append (list 'block nil)
                                                 (list
                                                  (append (append (list 'let
                                                                        (cons
                                                                         (list 'stack (list 'cl-containers:make-container (list 'quote 'cl-containers:stack-container)))
                                                                         (mapcar
                                                                          (lambda (v)
                                                                            (list v))
                                                                          (slot-value context 'locals))))
                                                                  (list (cons 'tagbody lisp-code)))))))))))

          (%eval code))))))

(defun emit-<class> (class)
  (let ((defclass-code (with-slots (name super fields) class
                         (list
                          'progn
                          (list
                           'defclass (intern name :openldk)
                           (if super (list (intern super :openldk)) (list))
                           (map 'list
                                (lambda (f)
                                  (list (intern (slot-value f 'name) :openldk)
                                        :initform nil
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

      (%clinit (classload "java/lang/Object" ".:jre8/"))
      (%clinit (classload "java/lang/ClassLoader" ".:jre8/"))
      (%clinit (classload "java/lang/String" ".:jre8/"))
      (%clinit (classload "java/lang/Class" ".:jre8/"))

      (if (not (eq 1 (length free-args)))
          (usage)
          (let* ((class (classload (car free-args) CLASSPATH)))
            (%clinit class)
            (%eval (list (intern (format nil "~A.main([Ljava/lang/String;)V" (slot-value class 'name)) :openldk) #())))))))
