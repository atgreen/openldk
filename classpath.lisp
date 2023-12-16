(in-package :openldk)

(defclass classpath-entry ()
  ())

(defclass jar-classpath-entry (classpath-entry)
  ((jarfile :initarg :jarfile)
   (zipfile :initform nil)
   (zipfile-entries)))

(defclass dir-classpath-entry (classpath-entry)
  ((dir :initarg :dir)))

(defmethod open-java-classfile ((cpe jar-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME."
  (with-slots (jarfile zipfile zipfile-entries) cpe
    (unless zipfile
      (setf zipfile (zip:open-zipfile jarfile))
      (setf zipfile-entries (zip:zipfile-entries zipfile)))
    (let ((ze (gethash (format nil "~A.class" classname) zipfile-entries)))
      (when ze
        (flexi-streams:make-in-memory-input-stream (zip:zipfile-entry-contents ze))))))

(defmethod open-java-classfile ((cpe dir-classpath-entry) classname)
  "Return an input stream for a java class, CLASSNAME."
  (with-slots (dir) cpe
    (let ((fqn (format nil "~A~A~A.class" dir (uiop:directory-separator-for-host) classname)))
      (when (uiop:file-exists-p fqn)
        (open fqn :direction :input :element-type '(unsigned-byte 8))))))
