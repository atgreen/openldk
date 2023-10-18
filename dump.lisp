(in-package :openldk)

(defun dump (id obj)
  "Dump OBJ, *CONTEXT* and *CLASSES* to disk in *DUMP-DIR*/[current-class]/ID."
  (format t "DUMP~%~A~%~A~%~A~%" (slot-value (slot-value *context* 'class) 'name) (slot-value *context* 'fn-name) id)
  (when *dump-dir*
    (let ((class-name (slot-value (slot-value *context* 'class) 'name))
          (fn-name (slot-value *context* 'fn-name)))
      (let ((ffn-name (if (alexandria:starts-with-subseq class-name fn-name)
                          fn-name
                          (format nil "~A~A" class-name fn-name))))
        (let* ((namestring (format nil "~A~A~A.~A" *dump-dir* (uiop:directory-separator-for-host)
                                   ffn-name id))
               (pathname (pathname-utils:parse-native-namestring namestring)))
          (uiop:ensure-all-directories-exist
           (list (pathname-utils:parent pathname)))
          (cl-store:store (list obj *context* *classes*) pathname))))))

(defun restore (filename)
  "Restore an object from the given file, as well as *CONTEXT* and
*CLASSES* at the time of dump."
  (let ((v (cl-store:restore
            (pathname-utils:parse-native-namestring filename))))
    (setf *context* (cadr v))
    (setf *classes* (caddr v))
    (car v)))

(alexandria:starts-with-subseq "sae" "sadfdb")
