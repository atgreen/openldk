(in-package :openldk)

(defun count-parameters (descriptor)
  "Count the number of parameters in a Java method descriptor."
  (let ((reading-complex-p nil)
        (count 0))
    (loop for ch across (subseq descriptor (position #\( descriptor) (position #\) descriptor))
          do (cond
               ((or (char= ch #\I) (char= ch #\J) (char= ch #\S) (char= ch #\B)
                    (char= ch #\C) (char= ch #\D) (char= ch #\F) (char= ch #\Z))
                (unless reading-complex-p (incf count)))
               ((char= ch #\L)
                (setf reading-complex-p t))
               ((char= ch #\;)
                (incf count)
                (setf reading-complex-p nil))
               (t nil)))
    count))

(defun parse-parameter-types (descriptor)
  "Parse the Java method descriptor and return a list of parameter types as strings."
  (let ((param-list nil)
        (index 0)
        (descriptor (subseq descriptor (position #\( descriptor) (position #\) descriptor))))
    (loop while (< index (length descriptor))
          do (let ((ch (char descriptor index)))
               (print index)
               (print ch)
               (cond
                 ;; For simple types
                 ((char= ch #\I) (push "int" param-list) (incf index))
                 ((char= ch #\J) (push "long" param-list) (incf index))
                 ((char= ch #\S) (push "short" param-list) (incf index))
                 ((char= ch #\B) (push "byte" param-list) (incf index))
                 ((char= ch #\C) (push "char" param-list) (incf index))
                 ((char= ch #\D) (push "double" param-list) (incf index))
                 ((char= ch #\F) (push "float" param-list) (incf index))
                 ((char= ch #\Z) (push "boolean" param-list) (incf index))

                 ;; For object types
                 ((char= ch #\L)
                  (let ((obj-end (position #\; descriptor :start index)))
                    (push (subseq descriptor (1+ index) obj-end) param-list)
                    (setf index (1+ obj-end))))

                 ;; For array types
                 ((char= ch #\[)
                  (incf index)
                  (loop until (not (member (char descriptor index) '(#\I #\J #\S #\B #\C #\D #\F #\Z #\[)))
                        do (incf index))
                  (when (char= (char descriptor index) #\L)
                    (setf index (position #\; descriptor :start index)))
                  (push (format nil "~a[]" (translate-type (char descriptor index))) param-list)
                  (incf index))

                 (t (incf index)))))
    (nreverse param-list))) ; Reverse the list before returning it, since we used push

(defun translate-type (type-char)
  (case type-char
    (#\I "int")
    (#\J "long")
    (#\S "short")
    (#\B "byte")
    (#\C "char")
    (#\D "double")
    (#\F "float")
    (#\Z "boolean")
    (t (string type-char))))

; (parse-parameter-types "(IS[Ljava/lang/String;)V")
