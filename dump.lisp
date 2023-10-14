(in-package :openldk)

(defun dump (id obj)
  "Dump OBJ, *CONTEXT* and *CLASSES* to disk in *DUMP-DIR*/[current-class]/ID."
  (when *dump-dir*
    (let* ((namestring (format nil "~A~A~A~A.~A" *dump-dir* (uiop:directory-separator-for-host)
                               (slot-value (slot-value *context* 'class) 'name)
                               (slot-value *context* 'fn-name) id))
           (pathname (pathname-utils:parse-native-namestring namestring)))
      (uiop:ensure-all-directories-exist
       (list (pathname-utils:parent pathname)))
      (cl-store:store (list obj *context* *classes*) pathname))))

(defun restore (filename)
  "Restore from the given file, as well as the *CONTEXT* at the time of dump."
  (let ((v (cl-store:restore filename)))
    (setf *context* (cadr v))
    (setf *classes* (caddr v))
    (car v)))
