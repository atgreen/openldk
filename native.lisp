(defmethod |java/lang/Object.registerNatives.()V| ()
  ())

(defmethod |java/lang/System.registerNatives.()V| ()
  ())

(defmethod |println.(Ljava/lang/String;)V| (stream string)
  (format t "~A~%" (slot-value string 'openldk::value)))
