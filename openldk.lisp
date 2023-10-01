(in-package :openldk)

(defvar *classes* (make-hash-table))

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

(defun classload (classname classpath)
  (let ((class (gethash classname *classes*)))
    (unless class
      (let ((fqfn (find-classpath-for-class classname classpath)))
        (if fqfn
            (let* ((class (read-classfile fqfn))
                   (super (let ((super (slot-value class 'super)))
                            (when super (classload super classpath))))
                   (<clinit>-method (find-if
                                     (lambda (method) (and (string= (slot-value method 'name) "<clinit>")
                                                           (string= (slot-value method 'descriptor) "()V")))
                                     (slot-value class 'methods))))
              (print <clinit>-method))
            (format t "ERROR: Can't find ~A on classpath ~A~%" classname CLASSPATH))))
    class))

(defun main ()
  (let ((CLASSPATH (uiop:getenv "CLASSPATH")))

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
            (classload (car free-args) CLASSPATH))))))
