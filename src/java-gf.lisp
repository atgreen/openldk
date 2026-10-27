;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2025  Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; This file is part of OpenLDK.

;;; OpenLDK is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.

;;; OpenLDK is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with OpenLDK; see the file COPYING.  If not, please see
;;; <http://www.gnu.org/licenses/>.

;;; Linking this library statically or dynamically with other modules is
;;; making a combined work based on this library.  Thus, the terms and
;;; conditions of the GNU General Public License cover the whole
;;; combination.

;;; As a special exception, the copyright holders of this library give
;;; you permission to link this library with independent modules to
;;; produce an executable, regardless of the license terms of these
;;; independent modules, and to copy and distribute the resulting
;;; executable under terms of your choice, provided that you also
;;; meet, for each linked independent module, the terms and conditions
;;; of the license of that module.  An independent module is a module
;;; which is not derived from or based on this library.  If you modify
;;; this library, you may extend this exception to your version of the
;;; library, but you are not obligated to do so.  If you do not wish
;;; to do so, delete this exception statement from your version.

(in-package :openldk)

;;; Custom generic function metaclass for Java single-dispatch methods.
;;; Uses a hash-table cache keyed on receiver class instead of PCL's
;;; discrimination nets, avoiding expensive dfun rebuilds when thousands
;;; of Java classes are loaded.

(defclass java-generic-function (standard-generic-function)
  ((dispatch-cache :initform (make-hash-table :test 'eq :synchronized t)
                   :accessor java-gf-dispatch-cache)
   (invoke-special-cache :initform (make-hash-table :test 'eq :synchronized t)
                         :accessor java-gf-invoke-special-cache)
   ;; Bumped whenever INVOKE-SPECIAL-CACHE is cleared, so the call sites
   ;; that keep an entry of their own (INVOKE-SPECIAL-AT) see it is stale.
   (invoke-special-version :initform 0 :type fixnum
                           :accessor java-gf-invoke-special-version)
   ;; First specializer -> its methods, for %JAVA-APPLICABLE-METHODS: NIL
   ;; until built, or :COMPLEX for methods it can't serve.
   (method-index :initform nil :accessor java-gf-method-index)
   (method-lock :initform (bordeaux-threads:make-recursive-lock "java-gf-methods")
                :reader java-gf-method-lock)
   (cache-lock :initform (bordeaux-threads:make-lock "java-gf-cache")
               :reader java-gf-cache-lock))
  (:documentation "Java single-dispatch generic function with synchronized method caches.")
  (:metaclass sb-mop:funcallable-standard-class))

(defvar *method-bodies* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "A compiled Java method's trampoline method -> the function holding its
body, which the trampoline calls (see %INSTALL-TRAMPOLINE).")

(defun %note-method-body (gf-symbol class-symbol body-symbol)
  "Record that GF-SYMBOL's primary method on CLASS-SYMBOL is a trampoline
calling BODY-SYMBOL."
  (let* ((gf (and (fboundp gf-symbol) (fdefinition gf-symbol)))
         (class (find-class class-symbol nil))
         (method (and gf class
                      (find-if (lambda (m)
                                 (and (null (method-qualifiers m))
                                      (eq (first (sb-mop:method-specializers m)) class)))
                               (sb-mop:generic-function-methods gf)))))
    (when method
      (setf (gethash method *method-bodies*) (fdefinition body-symbol)))))

(defun %install-trampoline (gf-symbol class-symbol impl-symbol nargs)
  "Make GF-SYMBOL's primary method on CLASS-SYMBOL call IMPL-SYMBOL, a
compiled Java method's body, with the receiver and NARGS more arguments,
and record it in *METHOD-BODIES*.  The method's function is a closure:
a DEFMETHOD would be compiled, which took a third of a small method's
compile time."
  (%ensure-java-gf gf-symbol (1+ nargs))
  (let* ((gf (fdefinition gf-symbol))
         (method (make-instance (sb-mop:generic-function-method-class gf)
                                :qualifiers nil
                                :lambda-list (cons (intern "this" :openldk)
                                                   (loop for i from 1 upto nargs
                                                         collect (intern (format nil "arg~A" i) :openldk)))
                                :specializers (cons (find-class class-symbol)
                                                    (make-list nargs :initial-element (find-class t)))
                                :function (lambda (args next-methods)
                                            (declare (ignore next-methods))
                                            (apply impl-symbol args)))))
    (add-method gf method)
    (setf (gethash method *method-bodies*) (fdefinition impl-symbol))
    method))

(defun %simple-java-method-p (method)
  "Does METHOD dispatch on its first argument's class alone, as a primary
or :AROUND method?"
  (let ((specializers (sb-mop:method-specializers method)))
    (and (member (method-qualifiers method) '(() (:around)) :test #'equal)
         (typep (first specializers) 'class)
         (every (lambda (s) (eq s (find-class t))) (rest specializers)))))

(defun %index-java-method (index method)
  "Add METHOD to INDEX, a METHOD-INDEX table, returning INDEX, or :COMPLEX."
  (cond ((%simple-java-method-p method)
         (push method (gethash (first (sb-mop:method-specializers method)) index))
         index)
        (t
         :complex)))

(defun %java-gf-method-index (gf)
  "GF's METHOD-INDEX, built if need be.  Call with its cache lock held."
  (or (java-gf-method-index gf)
      (setf (java-gf-method-index gf)
            (let ((index (make-hash-table :test 'eq)))
              (dolist (method (sb-mop:generic-function-methods gf) index)
                (when (eql (%index-java-method index method) :complex)
                  (return :complex)))))))

(defun %java-applicable-methods (gf class class-list)
  "COMPUTE-APPLICABLE-METHODS-USING-CLASSES of GF and CLASS-LIST, whose
first element is CLASS and the rest T: GF's methods on CLASS and its
superclasses, most specific first.  PCL's tests every method of GF, and
toString() or <init>() have thousands; this looks up the few classes
CLASS inherits from."
  (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
   (let ((index (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
                 (%java-gf-method-index gf))))
    (cond ((eql index :complex)
           (closer-mop:compute-applicable-methods-using-classes gf class-list))
          (t
           (unless (sb-mop:class-finalized-p class)
            (sb-mop:finalize-inheritance class))
           (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
            (loop for c in (sb-mop:class-precedence-list class)
                  append (gethash c index))))))))

(defun %compute-java-effective-method (gf class)
  "Compute an effective method function, called with GF's arguments, for GF
dispatching on CLASS.
Returns (values FUNCTION APPLICABLE-P); when APPLICABLE-P is NIL the
function signals no-applicable-method and must not be cached (the class
may still be a bare stub that a later classload fleshes out)."
  (let* ((lambda-list (closer-mop:generic-function-lambda-list gf))
         (nargs (length lambda-list))
         (class-list (cons class
                          (make-list (max 0 (1- nargs))
                                     :initial-element (find-class 't)))))
    (multiple-value-bind (methods definitive-p)
        (%java-applicable-methods gf class class-list)
      (declare (ignore definitive-p))
      (if (null methods)
          ;; No applicable methods — signal no-applicable-method
          ;; (preserves null → NullPointerException via native.lisp)
          (values (lambda (&rest args)
                    (apply #'no-applicable-method gf args))
                  nil)
          ;; Partition into :around and primary methods
          (let* ((around (serapeum:filter
                          (lambda (m) (equal (method-qualifiers m) '(:around)))
                          methods))
                 (primary (serapeum:filter
                           (lambda (m) (null (method-qualifiers m)))
                           methods))
                 (chain (append around primary))
                 ;; A compiled Java method's trampoline, with nothing around
                 ;; it: call its body directly.  It never calls
                 ;; call-next-method, so the rest of the chain can't run, and
                 ;; going through PCL's method function -- which unpacks the
                 ;; arguments for the fast method function -- cost a tenth of
                 ;; the Kotlin compiler's time.
                 (body (and (null around) primary
                            (gethash (first primary) *method-bodies*))))
            (when body
              (return-from %compute-java-effective-method
                (values body t)))
            (if (null chain)
                (values (lambda (&rest args)
                          (apply #'no-applicable-method gf args))
                        nil)
                (let ((first-mf (closer-mop:method-function (first chain)))
                      (rest-chain (rest chain)))
                  (values (lambda (&rest args)
                            (funcall first-mf args rest-chain))
                          t))))))))

(defun %invalidate-java-gf-cache (gf specializer)
  "Invalidate GF's dispatch cache entries affected by a method change on
SPECIALIZER: only receivers that are subtypes of it can see a different
applicable-method set.  Non-class specializers flush everything."
  (let ((cache (java-gf-dispatch-cache gf))
        (special-cache (java-gf-invoke-special-cache gf)))
    (flet ((remove-subtypes (table)
             ;; Remove TABLE's entries for subtypes of SPECIALIZER; true if any.
             (let (stale)
               (maphash (lambda (k v)
                          (declare (ignore v))
                          (when (subtypep k specializer)
                            (push k stale)))
                        table)
               (dolist (k stale) (remhash k table))
               stale)))
      (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
        (cond
          ((typep specializer 'class)
           (remove-subtypes cache)
           ;; An invokespecial entry for an owner class sees only methods
           ;; on it and its superclasses.  Every class loaded adds its
           ;; constructors to <init>'s generic function, and flushing every
           ;; entry for each made computing them a tenth of Spring Boot's
           ;; startup.
           (when (remove-subtypes special-cache)
             (incf (java-gf-invoke-special-version gf))))
          (t
           (clrhash cache)
           (clrhash special-cache)
           (incf (java-gf-invoke-special-version gf))))))))

(defmethod sb-mop:add-method :around ((gf java-generic-function) method)
  ;; SBCL replaces a method by removing its predecessor first. Keep
  ;; dispatch snapshots out of that temporary gap, including the hooks.
  (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
    (call-next-method)))

(defmethod sb-mop:remove-method :around ((gf java-generic-function) method)
  (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
    (call-next-method)))

(defmethod sb-mop:add-method :after ((gf java-generic-function) method)
  (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
    (let ((index (java-gf-method-index gf)))
      (when (hash-table-p index)
        (setf (java-gf-method-index gf) (%index-java-method index method)))))
  (%invalidate-java-gf-cache gf (first (sb-mop:method-specializers method))))

(defmethod sb-mop:remove-method :after ((gf java-generic-function) method)
  (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
    (let ((index (java-gf-method-index gf)))
      (if (hash-table-p index)
          (let ((specializer (first (sb-mop:method-specializers method))))
            (alexandria:removef (gethash specializer index) method))
          ;; A complex method may have been the one removed: rebuild.
          (setf (java-gf-method-index gf) nil))))
  (%invalidate-java-gf-cache gf (first (sb-mop:method-specializers method))))

(defmethod closer-mop:compute-discriminating-function ((gf java-generic-function))
  "Return a discriminating function that dispatches via a hash-table cache
keyed on the receiver's class.  SBCL's update-dfun calls this whenever the
method set changes; invalidation is handled surgically by the
add-method/remove-method :after hooks above, so the caches persist —
flushing them wholesale here made every lazy class load recompute hot
dispatch (equals/hashCode/toString) from scratch."
  (let ((cache (java-gf-dispatch-cache gf))
        (lock (java-gf-cache-lock gf)))
    (flet ((miss (args)
             ;; Compute and cache the effective method for the receiver's
             ;; class, then apply it.
             (let ((class (class-of (first args))))
               (let ((emfun
                       (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
                         (multiple-value-bind (function applicable-p)
                             (%compute-java-effective-method gf class)
                           (when applicable-p
                             (bordeaux-threads:with-lock-held (lock)
                               (setf (gethash class cache) function)))
                           function))))
                 ;; Calling a method may compile it and load other classes.
                 ;; Release the method lock before executing Java code.
                 (apply emfun args)))))
      ;; One discriminating function per arity up to four arguments -- most
      ;; Java methods' -- so a call conses no argument list; a cached body
      ;; (see %COMPUTE-JAVA-EFFECTIVE-METHOD) is then called as is.
      (macrolet ((dispatcher (&rest params)
                   `(lambda ,params
                      (if-let ((emfun (gethash (class-of ,(first params)) cache)))
                        (funcall emfun ,@params)
                        (miss (list ,@params))))))
        (case (length (closer-mop:generic-function-lambda-list gf))
          (1 (dispatcher a))
          (2 (dispatcher a b))
          (3 (dispatcher a b c))
          (4 (dispatcher a b c d))
          (t (lambda (&rest args)
               (if-let ((emfun (gethash (class-of (first args)) cache)))
                 (apply emfun args)
                 (miss args)))))))))


;;; Method names carry their return types, as the JVM's do, but some
;;; hand-written natives, and Lisp code calling Java methods, use names
;;; without them.  Each full-named generic function is registered under its
;;; short name, and a call that finds no applicable method in one forwards
;;; to the other (see NO-APPLICABLE-METHOD in native/support.lisp).

(defvar *full-gfs-by-short-name* (make-hash-table :test #'equal :synchronized t)
  "Short method name (no return type) -> full-named generic function symbols.")

(defun %register-full-gf (symbol)
  "Index a full Java method SYMBOL under its name without the return type."
  (let* ((name (symbol-name symbol))
         (close (position #\) name)))
    (when (and close (< (1+ close) (length name)))
      (pushnew symbol (gethash (serapeum:take (1+ close) name) *full-gfs-by-short-name*)))))

(defun %ensure-java-gf-1 (symbol nargs)
  "Create a Java generic function named SYMBOL with NARGS required arguments."
  (unless (fboundp symbol)
    (ensure-generic-function symbol
                             :generic-function-class 'java-generic-function
                             :lambda-list (cons (intern "this" :openldk)
                                                (loop for i from 1 below nargs
                                                      collect (intern (format nil "arg~A" i) :openldk))))))

(defun %ensure-java-gf (symbol nargs)
  "Make SYMBOL a Java method generic function of NARGS arguments (receiver
included) unless it already names a function, and register it.  The
generic function of its short name exists too, for Lisp code calling it."
  (%ensure-java-gf-1 symbol nargs)
  (let* ((name (symbol-name symbol))
         (close (position #\) name)))
    (when (and close (< (1+ close) (length name)))
      (%ensure-java-gf-1 (intern (serapeum:take (1+ close) name) (symbol-package symbol)) nargs)))
  (%register-full-gf symbol)
  symbol)

(defun %adopt-native-short-methods ()
  "For each hand-written native method named without its return type and
specialized on a loaded Java class, give the generic function of its full
name (the class's method of that name and arguments) a method forwarding
to it, unless it has one for that class.  Forwarding on a missing method
can't serve these: a method the full-named generic function has for a
superclass would shadow the native, as the bytecode of
sun.misc.Unsafe.getLongVolatile, calling itself on the
jdk.internal.misc.Unsafe instance OpenLDK's natives handle, did.  Run once
the runtime's classes are loaded."
  (do-symbols (short (find-package :openldk))
    (let* ((name (symbol-name short))
           (close (position #\) name)))
      (when (and close (= (1+ close) (length name))
                 (eq (symbol-package short) (find-package :openldk))
                 (fboundp short)
                 (typep (fdefinition short) 'generic-function))
        (dolist (method (closer-mop:generic-function-methods (fdefinition short)))
          (let* ((specializers (closer-mop:method-specializers method))
                 (class (first specializers))
                 (class-name (and (typep class 'class) (class-name class)))
                 (bin-name (and (symbolp class-name) (symbol-name class-name))))
            (when (and (null (method-qualifiers method))
                       bin-name (find #\/ bin-name)
                       (gethash bin-name *ldk-classes-by-bin-name*))
              (let ((full-name (%resolve-method-name bin-name name)))
                (unless (string= full-name name)
                  (let* ((full (intern full-name :openldk))
                         (nargs (length specializers)))
                    (%ensure-java-gf full nargs)
                    (let ((full-gf (fdefinition full)))
                      (when (and (typep full-gf 'generic-function)
                                 (= nargs (length (closer-mop:generic-function-lambda-list full-gf)))
                                 (notany (lambda (m)
                                           (and (null (method-qualifiers m))
                                                (eq class (first (closer-mop:method-specializers m)))))
                                         (closer-mop:generic-function-methods full-gf)))
                        (let ((params (loop for i from 1 below nargs
                                            collect (intern (format nil "ARG~A" i) :openldk))))
                          ;; The loaded Java class determines this method's specializer.
                          (ignore-errors
                           (eval `(defmethod ; lint:suppress eval-usage ,full ((this ,class-name) ,@params)
                                    (,short this ,@params)))))))))))))))))

(defun %java-dispatchable-gf-p (gf)
  "True when GF, a plain generic function, dispatches as a
JAVA-GENERIC-FUNCTION would: on its first argument's class, through
primary and :AROUND methods of required arguments only."
  (and (typep gf 'standard-generic-function)
       (not (typep gf 'java-generic-function))
       (eq (sb-pcl::method-combination-type-name
            (sb-mop:generic-function-method-combination gf))
           'standard)
       (let ((lambda-list (sb-mop:generic-function-lambda-list gf)))
         (and lambda-list
              (notany (lambda (p) (member p lambda-list-keywords)) lambda-list)))
       (every (lambda (m)
                (let ((specializers (sb-mop:method-specializers m)))
                  (and (member (method-qualifiers m) '(() (:around)) :test #'equal)
                       (typep (first specializers) 'class)
                       (every (lambda (s) (eq s (find-class t))) (rest specializers)))))
              (sb-mop:generic-function-methods gf))))

(defun %adopt-standard-java-gfs ()
  "Replace the plain generic functions of Java methods with JAVA-GENERIC-FUNCTIONs.
A hand-written native's DEFMETHOD makes its generic function before the
runtime's do, and so toString(), equals(), hashCode() and the like were
plain: each class loaded added methods to them, and PCL recomputed their
dispatch over thousands of methods -- 26 of Spring Boot's 65 seconds of
startup.  Run once the natives are loaded."
  (let ((openldk (find-package :openldk)))
    (do-symbols (symbol openldk)
      (when (and (eq (symbol-package symbol) openldk)
                 (fboundp symbol)
                 (find #\( (symbol-name symbol)))
        (let ((gf (fdefinition symbol)))
          (when (%java-dispatchable-gf-p gf)
            ;; SBCL can't CHANGE-CLASS a generic function, so move its
            ;; methods to a new one; callers find it through SYMBOL.
            (let ((new (make-instance 'java-generic-function
                                      :name symbol
                                      :lambda-list (sb-mop:generic-function-lambda-list gf)
                                      :method-class (sb-mop:generic-function-method-class gf))))
              (dolist (method (sb-mop:generic-function-methods gf))
                (remove-method gf method)
                (add-method new method))
              (setf (fdefinition symbol) new))))))))
