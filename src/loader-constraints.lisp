;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;; Copyright (C) 2026  Anthony Green <green@moxielogic.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0
;;;
;;; Initiating-loader identities and descriptor constraints (JVMS 5.3.4/5.4).

(in-package :openldk)

;;; JVMS 5.3.4: constraints relate initiating loaders, not just definitions.
;;; Never load a descriptor type to check a constraint: it may be defined later.
(defvar *loader-constraint-lock* (bt:make-recursive-lock "Java loader constraints"))
(defvar *initiated-classes* (make-hash-table :test #'equal)
  "Bootstrap initiations; other loaders retain their own initiation tables.")
(defvar *loader-constraints* (make-hash-table :test #'equal)
  "Equivalence groups of loader IDs, never strong loader references.")
(defvar *member-constraint-results* (make-hash-table :test #'eq :weakness :key))

(defun %constraint-loader (loader)
  "All temporary bootstrap metadata denotes the same JVM loader identity."
  (and loader (not (zerop (id loader))) (id loader)))

(defun %constraint-initiations (identity)
  "The initiation table of a live loader, or NIL for a collected identity."
  (if (null identity) *initiated-classes*
      (when-let ((loader (gethash identity *ldk-loaders-by-id*)))
        (slot-value loader 'initiated-classes))))

(defun %initiated-class (name loader)
  "A definition initiated by LOADER, including delegated definitions."
  (sb-sys:with-pinned-objects (loader)
    (bt:with-recursive-lock-held (*loader-constraint-lock*)
      (when-let ((table (%constraint-initiations (%constraint-loader loader))))
        (gethash name table)))))

(defun %constraint-conflict-p (name loaders &optional candidate)
  "Whether LOADERS have initiated distinct definitions of NAME. Lock is held."
  (let ((known candidate))
    (dolist (identity loaders)
      (when-let ((table (%constraint-initiations identity)))
        (when-let ((class (gethash name table)))
          (when (and known (not (eq known class)))
            (return-from %constraint-conflict-p t))
          (setf known class))))))

(defun %record-initiating-class (class loader)
  "Record initiation atomically, retracting the prospective record on conflict."
  (sb-sys:with-pinned-objects (loader)
    (let* ((name (name class)) (identity (%constraint-loader loader))
           (conflict (bt:with-recursive-lock-held (*loader-constraint-lock*)
                       (let* ((table (%constraint-initiations identity))
                              (old (gethash name table))
                              (group (find identity (gethash name *loader-constraints*) :test #'member)))
                         (cond ((or (and old (not (eq old class)))
                                    (%constraint-conflict-p name group class)) t)
                               (t (setf (gethash name table) class) nil))))))
      (when conflict
        (%throw '|java/lang/LinkageError| (format nil "Loader constraint violated for ~A" name)))))
  class)

(defun %impose-loader-constraint (name left right)
  "Join equivalence sets only after checking their complete transitive closure."
  (let ((left (%constraint-loader left)) (right (%constraint-loader right)))
    (when (and (not (eq left right))
               (bt:with-recursive-lock-held (*loader-constraint-lock*)
                 (let* ((groups (gethash name *loader-constraints*))
                        (a (find left groups :test #'member))
                        (b (find right groups :test #'member)))
                   (unless (and a (eq a b))
                     (let ((merged (remove-duplicates (append (or a (list left)) (or b (list right))))))
                       (cond ((%constraint-conflict-p name merged) t)
                             (t (setf (gethash name *loader-constraints*)
                                      (cons merged (remove a (remove b groups))))
                                nil)))))))
      (%throw '|java/lang/LinkageError| (format nil "Loader constraint violated for ~A" name)))))

(defun %forget-loader-constraints (identity)
  "Discard a dead identity without splitting constraints among surviving loaders."
  (bt:with-recursive-lock-held (*loader-constraint-lock*)
    (maphash (lambda (name groups)
               (if-let ((remaining (loop for group in groups
                                      for live = (remove identity group :test #'eql)
                                      when (rest live) collect live)))
                 (setf (gethash name *loader-constraints*) remaining)
                 (remhash name *loader-constraints*)))
             *loader-constraints*)))

(defun %descriptor-class-names (descriptor)
  "Reference element names in a field or method descriptor, without array ranks."
  (loop for start = (position #\L descriptor) then (position #\L descriptor :start (1+ end))
        while start
        for end = (position #\; descriptor :start start)
        collect (subseq descriptor (1+ start) end)))

(defun %constrain-descriptor (descriptor left right)
  "Constrain every reference element type without loading any of those types."
  (unless (eq (%constraint-loader left) (%constraint-loader right))
    (dolist (name (%descriptor-class-names descriptor))
      (%impose-loader-constraint name left right))))

(defun %constraint-field-owner (class name descriptor)
  "JVMS field lookup: own declaration, superinterfaces, then superclass."
  (let ((seen nil))
    (labels ((visit (current)
               (when (and current (not (member current seen)))
                 (push current seen)
                 (or (when (find-if (lambda (field) (and (string= name (name field))
                                                        (string= descriptor (descriptor field))))
                                    (fields current))
                       current)
                     (loop for parent across (or (interfaces current) #())
                           thereis (visit (%resolve-class parent (ldk-loader current))))
                     (and (super current) (visit (%resolve-class (super current) (ldk-loader current))))))))
      (visit class))))

(defstruct (member-linkage-site (:constructor %make-member-linkage-site (caller index)))
  caller index outcome)

(defun %member-linkage-site (caller index)
  "Share a stable resolution cell across every use of a constant-pool entry."
  (bt:with-recursive-lock-held (*loader-constraint-lock*)
    (let ((table (%owner-cache-table *member-constraint-results* caller 'eql)))
      (or (gethash index table)
          (setf (gethash index table) (%make-member-linkage-site caller index))))))

(defun %publish-member-linkage-outcome (caller index outcome)
  "Publish the first outcome, after the corresponding constraint updates."
  (let ((site (%member-linkage-site caller index)))
    (bt:with-recursive-lock-held (*loader-constraint-lock*)
      (or (member-linkage-site-outcome site)
          (progn
            (sb-thread:barrier (:write))
            (setf (member-linkage-site-outcome site) outcome))))))

(declaim (inline %check-member-constraint-site))
(defun %check-member-constraint-site (site)
  "The compiled fast path needs neither a hash lookup nor a runtime-wide lock."
  (let ((outcome (member-linkage-site-outcome site)))
    (sb-thread:barrier (:read))
    (case outcome
      (:allowed nil)
      ((nil) (%check-member-loader-constraints (member-linkage-site-caller site) (member-linkage-site-index site)))
      (otherwise (error outcome)))))

(defun %check-member-loader-constraints (caller index)
  "Resolve a CP member's descriptor constraints once, retaining linkage failures."
  (let ((cached (member-linkage-site-outcome (%member-linkage-site caller index))))
    (sb-thread:barrier (:read))
    (when cached
      (unless (eql cached :allowed) (error cached))
      (return-from %check-member-loader-constraints nil))
    ;; Do not hold the registry lock while invoking arbitrary class-loader code.
    (handler-case
        (multiple-value-bind (owner name descriptor)
            (%v-member (constant-pool caller) index
                       '(or constant-field-reference constant-method-reference constant-interface-method-reference))
          (let* ((target (%resolve-class owner (ldk-loader caller)))
                 (field-p (typep (aref (constant-pool caller) index) 'constant-field-reference))
                 (declaring (and target
                                 (if field-p (%constraint-field-owner target name descriptor)
                                     (if (string= name "<init>") target
                                         (%find-declaring-class target (concatenate 'string name descriptor)))))))
            (unless target
              (%throw '|java/lang/NoClassDefFoundError| owner))
            (when (and target (java-class caller) (java-class target))
              (%check-bytecode-class-access (java-class caller) (java-class target)))
            (when declaring
              (%constrain-descriptor descriptor (ldk-loader caller) (ldk-loader declaring)))
            (let ((outcome (%publish-member-linkage-outcome caller index :allowed)))
              (unless (eql outcome :allowed) (error outcome)))))
      (|condition-java/lang/LinkageError| (condition)
        (let ((outcome (%publish-member-linkage-outcome caller index condition)))
          (unless (eql outcome :allowed) (error outcome))))))
  nil)

(defun %constraint-parents (class)
  "All distinct superclasses and superinterfaces, using their defining loaders."
  (let ((parents nil))
    (labels ((visit (current)
               (when (and current (not (member current parents)))
                 (push current parents)
                 (when (super current)
                   (visit (%resolve-class (super current) (ldk-loader current))))
                 (loop for interface across (or (interfaces current) #())
                       do (visit (%resolve-class interface (ldk-loader current)))))))
      (when (super class) (visit (%resolve-class (super class) (ldk-loader class))))
      (loop for interface across (or (interfaces class) #())
            do (visit (%resolve-class interface (ldk-loader class)))))
    parents))

(defun %constraint-instance-method (class name descriptor)
  "An instance method eligible to override a public interface method."
  (find-if (lambda (method)
             (and (string= name (name method)) (string= descriptor (descriptor method))
                  (not (static-p method)) (not (logbitp 1 (access-flags method)))))
           (methods class)))

(defun %constraint-selected-owner (class name descriptor)
  "Select an inherited implementation, excluding private/static lookalikes."
  (or (loop for current = class then (and (super current) (%resolve-class (super current) (ldk-loader current)))
            while current
            when (%constraint-instance-method current name descriptor) return current)
      (let* ((interfaces (loop for parent in (%constraint-parents class)
                               when (and (interface-p parent) (%constraint-instance-method parent name descriptor))
                                 collect parent))
             (maximal (remove-if (lambda (parent)
                                   (some (lambda (other) (member parent (%constraint-parents other))) interfaces))
                                 interfaces))
             (concrete (remove-if (lambda (parent) (abstract-p (%constraint-instance-method parent name descriptor)))
                                  maximal)))
        (when (= (length concrete) 1) (first concrete)))))

(defun %prepare-loader-constraints (class)
  "Impose overriding and inherited interface-implementation constraints (5.4.2)."
  (let ((parents (%constraint-parents class)))
    (loop for method across (methods class)
          unless (or (static-p method) (logbitp 1 (access-flags method))
                     (member (name method) '("<init>" "<clinit>") :test #'string=))
            do (dolist (parent parents)
                 (let ((inherited (find-if (lambda (candidate)
                                            (and (string= (name candidate) (name method))
                                                 (string= (descriptor candidate) (descriptor method))))
                                          (methods parent))))
                   (when (and inherited (not (static-p inherited))
                              (not (logbitp 1 (access-flags inherited)))
                              (or (logtest #x5 (access-flags inherited)) (%same-runtime-package-p class parent)))
                     (%constrain-descriptor (descriptor method) (ldk-loader class) (ldk-loader parent))))))
    (dolist (interface parents)
      (when (interface-p interface)
        (loop for method across (methods interface)
              unless (or (static-p method) (logbitp 1 (access-flags method)))
                do (when-let ((selected (%constraint-selected-owner class (name method) (descriptor method))))
                     (%constrain-descriptor (descriptor method) (ldk-loader selected) (ldk-loader interface))))))))

(defun %forget-failed-class-definition (class loader)
  "Retract a failed definition without removing unrelated loader registrations."
  (bt:with-recursive-lock-held (*loader-constraint-lock*)
    (let ((table (%constraint-initiations (%constraint-loader loader))))
      (when (eq class (gethash (name class) table)) (remhash (name class) table))))
  (let ((java (java-class class)))
    (dolist (table (list (slot-value loader 'ldk-classes-by-bin-name)
                        (slot-value loader 'ldk-classes-by-fq-name)
                        *ldk-classes-by-bin-name* *ldk-classes-by-fq-name*))
      (maphash (lambda (key value) (when (eq value class) (remhash key table))) table))
    (when java
      (dolist (table (list (slot-value loader 'java-classes-by-bin-name)
                          (slot-value loader 'java-classes-by-fq-name)
                          *java-classes-by-bin-name* *java-classes-by-fq-name*))
        (maphash (lambda (key value) (when (eq value java) (remhash key table))) table)))))
