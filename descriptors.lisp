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

; (count-parameters "(ISLjava/lang/String;)V")
