

(progn
 (defmethod openldk::|<init>()| ((openldk::|this| openldk::x))
   (setf openldk::*force-this-to-be-used* openldk::|this|)
   "bridge=NIL"
   (let ((openldk::|condition-cache|)
         (openldk::|s{1}|)
         (openldk::|local-0| openldk::|this|)
         (openldk::|local-1|)
         (openldk::|local-2|)
         (openldk::|local-3|))
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
         (return))))))


(progn
 (defmethod openldk::|getY()| ((openldk::|this| openldk::x))
   (setf openldk::*force-this-to-be-used* openldk::|this|)
   "bridge=NIL"
   (let ((openldk::|condition-cache|)
         (openldk::|s{1}|)
         (openldk::|local-0| openldk::|this|)
         (openldk::|local-1|)
         (openldk::|local-2|)
         (openldk::|local-3|))
     (block nil
       (tagbody
        |branch-target-0|
         (setf openldk::|s{1}| (make-instance 'openldk::y))
         (destructuring-bind
             (method . openldk::next)
             (sb-mop:compute-applicable-methods-using-classes
              #'openldk::|<init>(I)| (list #<standard-class openldk::y> t t))
           (let ((openldk::fn (sb-mop:method-function method)))
             (funcall openldk::fn (list openldk::|s{1}| 1) openldk::next)))
         (let ((openldk::result openldk::|s{1}|))
           (cond
            (openldk::*debug-trace-args*
             (format t "~&~V@A <~A> trace: ~A result = ~A~%"
                     openldk::*call-nesting-level* "*"
                     openldk::*call-nesting-level* "getY()" openldk::result))
            (openldk::*debug-trace*
             (format t "~&~V@A <~A> trace: ~A~%" openldk::*call-nesting-level*
                     "*" openldk::*call-nesting-level* "getY()")))
           (return-from openldk::|getY()| openldk::result)))))))
