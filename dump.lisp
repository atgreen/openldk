(in-package :openldk)

(defun dump (id obj)
  "Dump OBJ to disk in *DUMP-DIR*/[current-class]/ID, and *CONTEXT* beside it."
  (print *dump-dir*)
  (when *dump-dir*
    (print "OKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKKK!!!!!!!!!!!!!!!!")
    (flet ((directory-from-filename (filename)
             (let ((path (pathname filename)))
               (namestring (make-pathname :host (pathname-host path)
                                          :device (pathname-device path)
                                          :directory (pathname-directory path)
                                          :name nil
                                          :type nil
                                          :version nil)))))

      (let* ((path (format nil "~A~A~A.~A" *dump-dir* (uiop:directory-separator-for-host)
                           (slot-value (slot-value *context* 'class) 'name) id))
             (context-path (format nil "~A.context" path)))
        (uiop:ensure-all-directories-exist
         (list (directory-from-filename path)))
        (cl-store:store obj path)
        (cl-store:store *context* context-path)))))

(defun restore (filename)
  "Restore from the given file, as well as the *CONTEXT* at the time of dump."
  (setf *context* (cl-store:restore (format nil "~A.context" filename)))
  (cl-store:restore filename))
