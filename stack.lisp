(in-package :openldk)

(defmacro push-item (stack item)
  `(progn (push ,item ,stack)
	  (format t "--- push ~A~%" ,stack)))

(defmacro pop-item (stack)
  `(progn (format t "-- pop ~A~%" ,stack)
	  (pop ,stack)))
