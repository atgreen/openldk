(in-package :openldk)

(defmacro push-item (stack item)
  (if *debug-stack*
      `(progn (push ,item ,stack)
              (format t "--- push ~A~%" ,stack))
      `(push ,item ,stack)))

(defmacro pop-item (stack)
  (if *debug-stack*
      `(progn (format t "-- pop ~A~%" ,stack)
              (pop ,stack))
      `(pop ,stack)))
