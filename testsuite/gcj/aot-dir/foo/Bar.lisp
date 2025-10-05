

(progn
 (defmethod openldk::|<init>(I)|
            ((openldk::|this| openldk::|foo/Bar|) openldk::|arg1|)
   (setf openldk::*force-this-to-be-used* openldk::|this|)
   "bridge=NIL"
   (let ((openldk::|condition-cache|)
         (openldk::|s{3}|)
         (openldk::|s{2}|)
         (openldk::|s{1}|)
         (openldk::|local-0| openldk::|this|)
         (openldk::|local-1| openldk::|arg1|)
         (openldk::|local-2|)
         (openldk::|local-3|)
         (openldk::|local-4|))
     (block nil
       (tagbody
        |branch-target-0|
         (setf openldk::|s{1}| openldk::|local-0|)
         (destructuring-bind
             (method . openldk::next)
             (sb-mop:compute-applicable-methods-using-classes
              #'openldk::|<init>()|
              (list #<standard-class openldk::|java/lang/Object|> t))
           (let ((openldk::fn (sb-mop:method-function method)))
             (funcall openldk::fn (list openldk::|s{1}|) openldk::next)))
         (setf openldk::|s{2}| openldk::|local-0|)
         (setf openldk::|s{3}| openldk::|local-1|)
         (setf (slot-value
                (let ((openldk::objref openldk::|s{2}|))
                  (when (null openldk::objref)
                    (error (format nil "Null Pointer Exception ~A" 6)))
                  openldk::objref)
                'openldk::|value|)
                 openldk::|s{3}|)
         (return))))))


(progn
 (defmethod openldk::|getValue()| ((openldk::|this| openldk::|foo/Bar|))
   (setf openldk::*force-this-to-be-used* openldk::|this|)
   "bridge=NIL"
   (let ((openldk::|condition-cache|)
         (openldk::|s{2}|)
         (openldk::|s{1}|)
         (openldk::|local-0| openldk::|this|)
         (openldk::|local-1|)
         (openldk::|local-2|)
         (openldk::|local-3|))
     (block nil
       (tagbody
        |branch-target-0|
         (setf openldk::|s{1}| openldk::|local-0|)
         (setf openldk::|s{2}|
                 (slot-value
                  (let ((openldk::objref openldk::|s{1}|))
                    (when (null openldk::objref)
                      (error (format nil "Null Pointer Exception ~A" 1)))
                    openldk::objref)
                  'openldk::|value|))
         (let ((openldk::result openldk::|s{2}|))
           (cond
            (openldk::*debug-trace-args*
             (format t "~&~V@A <~A> trace: ~A result = ~A~%"
                     openldk::*call-nesting-level* "*"
                     openldk::*call-nesting-level* "getValue()"
                     openldk::result))
            (openldk::*debug-trace*
             (format t "~&~V@A <~A> trace: ~A~%" openldk::*call-nesting-level*
                     "*" openldk::*call-nesting-level* "getValue()")))
           (return-from openldk::|getValue()| openldk::result)))))))
