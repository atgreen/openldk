(in-package :openldk)

(defclass |java/lang/Object| ()
  ())

(defclass |java/lang/String| (|java/lang/Object|)
  ((|value| :initform NIL :allocation :instance)
   (|hash| :initform NIL :allocation :instance)
   (|serialVersionUID| :initform NIL :allocation :class)
   (|serialPersistentFields| :initform NIL :allocation :class)
   (CASE_INSENSITIVE_ORDER :initform NIL :allocation :class)))

(defmethod print-object ((s |java/lang/String|) out)
  (print-unreadable-object (s out :type t)
    (format out "~S" (slot-value s '|value|))))

(defclass |java/lang/Throwable| (|java/lang/Object|)
  ())

(define-condition |condition-java/lang/Throwable| (error)
  ((objref)))

(define-condition |condition-java/lang/Exception|
    (|condition-java/lang/Throwable|)
  ((objref)))

(define-condition |condition-java/lang/ArithmeticException|
    (|condition-java/lang/Exception|)
  ((objref)))
