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
  (let ((c (classload "java/lang/SecurityManager" ".:jre8/")))
    (eval (list 'make-instance (list 'quote '|java/lang/SecurityManager|)))))

(defmethod |println(Ljava/lang/String;)V| (stream string)
  (format t "~A~%" (slot-value string '|value|)))

(defmethod |fillInStackTrace(I)Ljava/lang/Throwable;| ((|this| |java/lang/Throwable|) dummy)
  (let ((bt (trivial-backtrace:print-backtrace nil :output nil)))
    (print bt)))
