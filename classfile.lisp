(in-package :openldk)

(defclass constant-class-reference ()
  ((index :initarg :index)))

(defclass constant-string-reference ()
  ((index :initarg :index)))

(defclass constant-string ()
  ((value :initarg :value)))

(defclass constant-float ()
  ((value :initarg :value)))

(defclass constant-int ()
  ((value :initarg :value)))

(defclass constant-field-reference ()
  ((class-index :initarg :class-index)
   (type-descriptor-index :initarg :type-descriptor-index)))

(defclass constant-method-reference ()
  ((class-index :initarg :class-index)
   (method-descriptor-index :initarg :method-descriptor-index)))

(defclass constant-name-and-type-descriptor ()
  ((name-index :initarg :name-index)
   (type-descriptor-index :initarg :type-descriptor-index)))

(defun wrap-fast-read-sequence (vec buf &key (start 0) (end nil))
  "Must wrap this so it has the same signature as clhs' READ-SEQUENCE."
  (fast-io:fast-read-sequence vec buf start end))

(defclass <class> ()
  ((initialized-p :initform nil)
   (name :initarg :name)
   (super :initform nil)
   (fields)
   (methods)))

(defclass <method> ()
  ((name :initarg :name)
   (descriptor :initarg :descriptor)))

(defmethod initialize-instance :after ((this <method>) &key)
  (format t "INITIALIZING METHOD ~A~%" (slot-value this 'name)))

(defun read-classfile (filename)
  (let ((class (make-instance '<class>)))
    (with-slots (methods fields super) class
      (with-open-file (fin filename :element-type '(unsigned-byte 8))
        (fast-io:with-fast-input (fin-fast nil fin)
           (let ((bitio (bitio:make-bitio fin-fast #'fast-io:fast-read-byte #'wrap-fast-read-sequence)))
             (flet ((read-buffer (size)
                      (let ((buf (make-array size :element-type '(unsigned-byte 8))))
                        (bitio:read-bytes bitio buf :bit-endian :be :bits-per-byte 8)
                        buf))
                    (read-u1 () (bitio:read-one-byte bitio))
                    (read-u2 ()
                      (bitio:read-integer bitio
                                          :num-bytes 2
                                          :byte-endian :be
                                          :unsignedp t))
                    (read-u4 ()
                      (bitio:read-integer bitio
                                          :num-bytes 4
                                          :byte-endian :be
                                          :unsignedp t)))
               (let ((magic (bitio:read-integer bitio :unsignedp nil :byte-endian :be))
                     (minor-version (read-u2))
                     (major-version (read-u2))
                     (constant-pool-count (read-u2)))
                 (format t "~A~%" constant-pool-count)
                 (let ((constant-pool (make-array (1+ constant-pool-count))))
                   (dotimes (i (1- constant-pool-count))
                     (let ((tag (read-u1)))
                       (format t "~A: ~A~%" i tag)
                       (setf (aref constant-pool (1+ i))
                             ;; https://en.wikipedia.org/wiki/Java_class_file
                             (ccase tag
                               (1
                                (let* ((size (read-u2))
                                       (string
                                         (flexi-streams:octets-to-string (read-buffer size)
                                                                         :external-format :utf-8)))
                                  (make-instance 'constant-string
                                                 :value string)))
                               (3
                                (make-instance 'constant-int
                                               :value (read-u4)))
                               (4
                                (make-instance 'constant-float
                                               :value (read-u4)))
                               (7
                                (make-instance 'constant-class-reference
                                               :index (read-u2)))
                               (8
                                (make-instance 'constant-string-reference
                                               :index (read-u2)))
                               (9
                                (make-instance 'constant-field-reference
                                               :class-index (read-u2)
                                               :type-descriptor-index (read-u2)))
                               (10
                                (let ((class-index (read-u2))
                                      (method-descriptor-index (read-u2)))
                                  (make-instance 'constant-method-reference
                                                 :class-index class-index
                                                 :method-descriptor-index method-descriptor-index)))
                               (12
                                (let ((name-index (read-u2))
                                      (type-descriptor-index (read-u2)))
                                  (make-instance 'constant-name-and-type-descriptor
                                                 :name-index name-index
                                                 :type-descriptor-index type-descriptor-index)))
                               ))))

                   (let* ((access-flags (read-u2))
                          (this-class (read-u2))
                          (super-class (read-u2))
                          (interface-count (read-u2))
                          (interfaces (make-array interface-count :element-type '(unsigned-byte 16))))
                     (if (> super-class 0)
                         (setf super
                               (slot-value (aref constant-pool (slot-value (aref constant-pool super-class) 'index)) 'value)))

                     (format t "Interface count = ~A~%" interface-count)
                     (bitio:read-bytes bitio interfaces :bit-endian :be :bits-per-byte 16)

                     (let ((fields-count (read-u2)))
                       (format t "Field count = ~A~%" fields-count)
                       (dotimes (i fields-count)
                         (format t "Field #~A~%" i)
                         (let ((access-flags (read-u2))
                               (name-index (read-u2))
                               (descriptor-index (read-u2))
                               (attributes-count (read-u2)))
                           (dotimes (i attributes-count)
                             (let* ((attributes-name-index (read-u2))
                                    (attributes-length (read-u4))
                                    (info (read-buffer attributes-length)))))))
                       (let ((methods-count (read-u2)))
                         (setf methods (make-array methods-count))
                         (dotimes (i methods-count)
                           (format t "Method #~A~%" i)
                           (let* ((access-flags (read-u2))
                                  (name-index (read-u2))
                                  (descriptor-index (read-u2))
                                  (attributes-count (read-u2))
                                  (method (make-instance '<method>
                                                         :name (slot-value (aref constant-pool name-index) 'value)
                                                         :descriptor (slot-value (aref constant-pool descriptor-index) 'value))))
                             (setf (aref methods i) method)
                             (format t "name-index ~A: ~A~%" name-index (aref constant-pool name-index))
                             (format t "AtC ~A~%" attributes-count)
                             (dotimes (i attributes-count)
                               (format t "Attribute #~A~%" i)
                               (let* ((attributes-name-index (read-u2))
                                      (attributes-length (read-u4))
                                      (xxx (format t "al ~A~%" attributes-length))
                                      (info (read-buffer attributes-length)))))))
                         (let ((attributes-count (read-u2)))
                           (format t "AtC2 ~A~%" attributes-count)
                           (dotimes (i attributes-count)
                             (format t "Attribute2 #~A~%" i)
                             (let* ((attributes-name-index (read-u2))
                                    (attributes-length (read-u4))
                                    (xxx (format t "al ~A~%" attributes-length))
                                    (info (read-buffer attributes-length)))))))))
                   class)))))))))
