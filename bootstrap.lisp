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

#|
(defclass |java/lang/Class| (|java/lang/Object|)
  ((name :initarg :name)
   (class)))

(defmethod print-object ((c |java/lang/Class|) out)
  (print-unreadable-object (c out :type t)
    (format out "~A" (slot-value c 'name))))
|#
;      (%clinit (classload "java/lang/Class" ".:jre8/"))
