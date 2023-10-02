(in-package :openldk)

(defvar *classes* (make-hash-table :test 'equal))

(defvar *debug-codegen* nil)

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

(defclass <context> ()
  ((class :initarg :class)
   (pc :initform 0)
   (stack :initform (cl-containers:make-container 'cl-containers:stack-container))
   (code)))

(defmethod current-class-name ((context <context>))
  (slot-value (slot-value context 'class) 'name))

(defun :RETURN (context code)
  (with-slots (pc) context
    (incf pc)
    (list 'return)))

(defun :ACONST_NULL (context code)
  (with-slots (pc stack) context
    (incf pc)
    (cl-containers:push-item stack nil)))

(defun :ICONST_1 (context code)
  (with-slots (pc stack) context
    (incf pc)
    (cl-containers:push-item stack 1)
    nil))

(defun :ICONST_2 (context code)
  (with-slots (pc stack) context
    (incf pc)
    (cl-containers:push-item stack 2)
    nil))

(defun :POP (context code)
  (with-slots (pc stack) context
    (incf pc)
    (cl-containers:pop-item stack)
    nil))

(defun :LDC (context code)
  (with-slots (pc stack class) context
    (with-slots (constant-pool) class
      (let ((index (aref code (incf pc))))
        (incf pc)
        (cl-containers:push-item stack
                                 (emit (aref constant-pool index) constant-pool))
        nil))))

(defun :NEW (context code)
  (with-slots (pc stack class) context
    (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc))))
             (classname (emit (aref constant-pool index) constant-pool)))
        (format t ">> ~A~%" classname)))))

(defun :PUTSTATIC (context code)
  (with-slots (pc stack class) context
    (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc))))
             (field-reference (aref constant-pool index)))
        (incf pc)
        (let ((code (list 'setf (list 'slot-value
                                      (intern (format nil "+static-~A+" (intern (slot-value class 'name))))
                                      (list 'quote (intern (emit field-reference constant-pool))))
                          (cl-containers:pop-item stack))))
          code)))))

(defun :GETSTATIC (context code)
  (with-slots (pc stack class) context
    (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc)))))
        (multiple-value-bind (fieldname classname)
            (emit (aref constant-pool index) constant-pool)
          (incf pc)
          (cl-containers:push-item stack
                                   (list 'slot-value
                                         (intern (format nil "+static-~A+" (intern classname)))
                                         (list 'quote (intern fieldname)))))
        nil))))

(defun pop-args (num-args stack)
  (loop repeat num-args
        for item = (cl-containers:pop-item stack)
        until (null item)
        collect item into items
        finally (return (reverse items))))

(defun :INVOKESTATIC (context code)
  (with-slots (pc class stack) context
    (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc))))
             (method-reference (aref constant-pool index)))
        (incf pc)
        (let* ((descriptor
                 (slot-value (aref constant-pool
                                   (slot-value (aref constant-pool (slot-value method-reference 'method-descriptor-index)) 'type-descriptor-index))
                             'value))
               (parameter-count (count-parameters descriptor)))
          (cons (intern (format nil "~A.~A" (intern (slot-value class 'name)) (emit method-reference constant-pool)))
                (pop-args parameter-count stack)))))))

(defun :INVOKEVIRTUAL (context code)
  (with-slots (pc class stack) context
    (with-slots (constant-pool) class
      (let* ((index (+ (* (aref code (incf pc)) 256)
                       (aref code (incf pc))))
             (method-reference (aref constant-pool index)))
        (incf pc)
        (let* ((descriptor
                 (slot-value (aref constant-pool
                                   (slot-value (aref constant-pool (slot-value method-reference 'method-descriptor-index)) 'type-descriptor-index))
                             'value))
               (parameter-count (1+ (count-parameters descriptor))))
          (cons (intern (format nil "~A" (emit method-reference constant-pool)))
                (pop-args parameter-count stack)))))))

(defun invoke-method (context method)
  (let* ((code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
         (length (length code)))
    (with-slots (pc) context
      (let ((code (append (list 'block nil)
                          (loop
                            while (< pc length)
                            for result = (funcall (aref +opcodes+ (aref code pc)) context code)
                            unless (null result)
                            collect result))))
        (when *debug-codegen*
          (print code))
        (eval code)))))

(defun %compile-method (class-name method-index)
  (format t "compiling ~A~%" class-name)
  (let* ((class (gethash class-name *classes*))
         (method (aref (slot-value class 'methods) (1- method-index))))
    (let* ((code (slot-value (gethash "Code" (slot-value method 'attributes)) 'code))
           (length (length code))
           (context (make-instance '<context> :class class)))
      (with-slots (pc) context
        (let ((code (append
                     (list 'defmethod
                           (intern (format nil "~A~A" (slot-value method 'name) (slot-value method 'descriptor)))
                           (cons (list (intern "this") (intern (slot-value class 'name)))
                                 (loop for i from 1 upto (count-parameters (slot-value method 'descriptor))
                                       collect (intern (format nil "arg~A" i))))
                           (append (list 'block nil)
                                   (loop
                                     while (< pc length)
                                     for result = (funcall (aref +opcodes+ (aref code pc)) context code)
                                     unless (null result)
                                       collect result))))))
          (when *debug-codegen*
            (print code))
          (eval code))))))

(defun emit-<class> (class)
  (let ((defclass-code (with-slots (name super fields) class
                         (list
                          'progn
                          (list
                           'defclass (intern name)
                           (if super (list (intern super)) (list))
                           (map 'list
                                (lambda (f)
                                  (list (intern (slot-value f 'name))
                                        :allocation
                                        (if (eq 0 (logand 8 (slot-value f 'access-flags))) :instance :class)))
                                fields))
                          (list
                           'defparameter (intern (format nil "+static-~A+" (intern name)))
                           (list
                            'make-instance (list 'quote (intern name)))))))
        (methods-code
          (let ((method-index 0))
            (with-slots (name super methods) class
              (map 'list
                   (lambda (m)
                     (list 'defmethod (intern (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))
                           (cons (list (intern "this") (intern (slot-value (slot-value m 'class) 'name)))
                                 (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                       collect (intern (format nil "arg~A" i))))
                           (list '%compile-method (slot-value class 'name) (incf method-index))
                           (cons (intern (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))
                                 (cons (intern "this")
                                       (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                             collect (intern (format nil "arg~A" i)))))))
                   methods)))))
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
                              (when super (classload super classpath))))
                     (<clinit>-method (find-if
                                       (lambda (method) (and (string= (slot-value method 'name) "<clinit>")
                                                             (string= (slot-value method 'descriptor) "()V")))
                                       (slot-value class 'methods))))
                (let ((code (emit-<class> class)))
                  (when *debug-codegen*
                    (print code))
                  (eval code))
                (when (and <clinit>-method (not (slot-value class 'initialized-p)))
                  (invoke-method (make-instance '<context> :class class) <clinit>-method)
                  (setf (slot-value class 'initialized-p) t))
                class)
              (format t "ERROR: Can't find ~A on classpath ~A~%" classname CLASSPATH))))))

(defun main ()
  (let ((CLASSPATH (uiop:getenv "CLASSPATH"))
        (LDK_DEBUG (uiop:getenv "LDK_DEBUG")))

    (when LDK_DEBUG
      (when (find #\c LDK_DEBUG)
        (setf *debug-codegen* t)))

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
          (let* ((class (classload (car free-args) CLASSPATH)))
            (eval (list (intern "main([Ljava/lang/String;)V") (intern (format nil "+static-~A+" (slot-value class 'name))) #())))))))
#|
                 (main-method (find-if
                               (lambda (method) (and (string= (slot-value method 'name) "main")
                                                     (string= (slot-value method 'descriptor) "([Ljava/lang/String;)V")))
                               (slot-value class 'methods))))
            (if main-method
                (invoke-method (make-instance '<context> :class class) main-method)
                (format t "ERROR: class ~A has no main method." class)))))))
|#
