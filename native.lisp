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

(defmethod |println(Ljava/lang/String;)V| (stream string)
  (format t "~A~%" (slot-value string '|value|)))
