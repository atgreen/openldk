

(progn
 (defmethod openldk::|<init>()| ((openldk::|this| openldk::|SimpleTest|))
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
              (list (find-class 'openldk::|java/lang/Object|) t))
           (let ((openldk::fn (sb-mop:method-function method)))
             (funcall openldk::fn (list openldk::|s{1}|) openldk::next)))
         (return))))))


(progn
 (defun openldk::|SimpleTest.main([Ljava/lang/String;)| (openldk::|arg0|)
   "bridge=NIL"
   (let ((openldk::|condition-cache|)
         (openldk::|s{1}|)
         (openldk::|local-0| openldk::|arg0|)
         (openldk::|local-1|)
         (openldk::|local-2|)
         (openldk::|local-3|))
     (block nil
       (tagbody
        |branch-target-0|
         (unless (openldk::initialized-p #<openldk::<class> java/lang/System>)
           (openldk::|%clinit-java/lang/System|))
         (setf openldk::|s{1}|
                 (slot-value openldk::|+static-java/lang/System+|
                             'openldk::|out|))
         (funcall #'openldk::|println(Ljava/lang/String;)| openldk::|s{1}|
                  #<openldk::|java/lang/String| "Hello from AOT">)
         (return))))))
