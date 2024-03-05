(in-package :openldk)

(defun dump-method-dot (bloc)
  (when *dump-dir*
    (let ((class-name (slot-value (slot-value *context* 'class) 'name))
          (fn-name (slot-value *context* 'fn-name))
          (dt (make-hash-table)))
      (let ((ffn-name (substitute #\. #\/ (if (alexandria:starts-with-subseq class-name fn-name)
                                              fn-name
                                              (format nil "~A~A" class-name fn-name)))))
        (let* ((namestring (format nil "~A~A~A.dot" *dump-dir*
                                   (uiop:directory-separator-for-host)
                                   ffn-name))
               (pathname (pathname-utils:parse-native-namestring namestring)))
          (uiop:ensure-all-directories-exist
           (list (pathname-utils:parent pathname)))
          (with-open-file (stream pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
            (format stream "digraph code {~%graph [rankdir=TB];~%node [shape=box];~%")
            (dump-dot bloc dt stream)
            (format stream "}~%")))))))

(defun dump (id obj)
  "Dump OBJ, *CONTEXT* and *CLASSES* to disk in *DUMP-DIR*/[current-class]/ID."
  (when *dump-dir*
    (let ((class-name (slot-value (slot-value *context* 'class) 'name))
          (fn-name (slot-value *context* 'fn-name)))
      (let ((ffn-name (if (alexandria:starts-with-subseq class-name fn-name)
                          fn-name
                          (format nil "~A~A" class-name fn-name))))
        (let* ((namestring (format nil "~A~A~A.~A" *dump-dir*
                                   (uiop:directory-separator-for-host)
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
