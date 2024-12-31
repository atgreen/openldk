(in-package :openldk)

(defun |java/lang/Object.registerNatives()V| ()
  ())

(defun |java/lang/ClassLoader.registerNatives()V| ()
  ())

(defun |java/lang/System.registerNatives()V| ()
  ())

(defun |java/lang/Class.registerNatives()V| ()
  ())

(defun |java/lang/Class.desiredAssertionStatus0(Ljava/lang/Class;)Z| (class)
  (print "desiredAssertionStatus0")
  (print class)
  nil)

(defun |java/lang/Class.getSecurityManager()Ljava/lang/SecurityManager;| ()
  (print "java/lang/Class.getSecurityManager()Ljava/lang/SecurityManager;")
  (classload "java/lang/SecurityManager")
  (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|))))

(defmethod |println(Ljava/lang/String;)V| (stream string)
  (format t "~A~%" (slot-value string '|value|)))

(defmethod |println(I)V| (stream number)
  (format t "~A~%" number))

(defmethod |println(Ljava/lang/Object;)V| (stream object)
  (format t "~A~%" object))

(defmethod |fillInStackTrace(I)Ljava/lang/Throwable;| ((|this| |java/lang/Throwable|) dummy)
  (let ((bt (trivial-backtrace:print-backtrace nil :output nil)))
    (print bt)))

(defmethod |sun/reflect/Reflection.getCallerClass()Ljava/lang/Class;| ()
  (let* ((caller-string (format nil "~A" (third (sb-debug:backtrace-as-list))))
         (cstring (subseq caller-string 1 (position #\. caller-string))))
    (gethash cstring *java-classes*)))

(defmethod |java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (name initialize loader caller)
  (let ((lname (substitute #\/ #\. (slot-value name '|value|))))
    (print "==============================================================")
    (print lname)
    (maphash (lambda (k v) (print k)) *java-classes*)
    (or (gethash lname *java-classes*)
        (progn (%clinit (classload lname))
               (let ((java-class (make-instance '|java/lang/Class|)))
                 (setf (slot-value java-class '|name|)
                       (let ((s (make-instance '|java/lang/String|)))
                         (setf (slot-value s '|value|) name)))
                 (setf (slot-value java-class '|classLoader|) loader)
                 (setf (gethash lname *java-classes*) java-class)
                 java-class)))))
