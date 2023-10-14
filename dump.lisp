(in-package :openldk)

(defun dump (id obj)
  "Dump OBJ to disk in *DUMP-DIR*/[current-class]/ID, and *CONTEXT* beside it."
  (when *dump-dir*
    (flet ((directory-from-filename (filename)
             (let ((path (pathname filename)))
               (namestring (make-pathname :host (pathname-host path)
                                          :device (pathname-device path)
                                          :directory (pathname-directory path)
                                          :name nil
                                          :type nil
                                          :version nil)))))

      (let* ((path (format nil "~A~A~A~A.~A" *dump-dir* (uiop:directory-separator-for-host)
                           (slot-value (slot-value *context* 'class) 'name)
                           (slot-value *context* 'fn-name) id))
             (context-path (format nil "~A.context" path)))
        (uiop:ensure-all-directories-exist
         (list (directory-from-filename path)))
        (cl-store:store (list obj *context* *classes*)  path)))))

(defun restore (filename)
  "Restore from the given file, as well as the *CONTEXT* at the time of dump."
  (let ((v (cl-store:restore filename)))
    (setf *context* (cadr v))
    (setf *classes* (caddr v))
    (car v)))
