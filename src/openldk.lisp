;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: OPENLDK; Base: 10 -*-
;;;
;;; Copyright (C) 2024, 2025  Anthony Green <green@moxielogic.com>
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

(annot:enable-annot-syntax)

(in-package :openldk)

;; Methods compiled by older OpenLDKs stored THIS here on every call so
;; getCallerClass could find it in backtraces.  Caller classes now come
;; from the |%jimpl:Class.method| frame name, so nothing writes it; the
;; variable remains for previously generated AOT code that still does.
(defvar *force-this-to-be-used* nil)

(defvar *compact-method-length* 2000
  "Methods with more bytecode than this get compact code (see the
COMPACT-P slot of <context>).")

(defvar *methods-being-compiled* (make-hash-table :test #'equal :synchronized t)
  "Hash table tracking methods currently being compiled. Value is either T (compiling) or :DONE (compiled).")

(defvar *method-compilation-lock* (bt:make-lock "method-compilation-lock")
  "Lock to ensure atomic check-and-set for method compilation tracking.")

(defvar *method-compilation-cv* (bt:make-condition-variable :name "method-compilation-cv")
  "Condition variable to signal when a method compilation completes.")

;; ============================================================================
;; SIGQUIT (signal 3) handler for debugging hangs - dumps all thread stacks
;; ============================================================================

(defun %print-thread-backtrace (&key (stream *error-output*))
  "Print backtrace for the current thread to STREAM."
  (ignore-errors
    (sb-debug:print-backtrace :stream stream :print-thread t))
  (values))

(defun %dump-all-thread-stacks (&key (stream *error-output*))
  "Dump backtraces for all threads and fibers to STREAM. Called on SIGQUIT."
  (format stream "~&~80,,,'-@<~>~%")
  (format stream "Thread dump at ~A~%" (get-universal-time))
  (format stream "~80,,,'-@<~>~%")
  (let* ((all-threads (bt:all-threads))
         (current (bt:current-thread)))
    (format stream "~&Total threads: ~D~%~%" (length all-threads))
    ;; Print current thread's backtrace first
    (format stream "~&=== Current Thread: ~A ===~%" current)
    (%print-thread-backtrace :stream stream)
    ;; Then interrupt other threads to get their backtraces.  Capture each into
    ;; a string in the target's own context and wait (with a timeout) on a
    ;; semaphore it signals when done: interruptible threads (e.g. a 0%-CPU block
    ;; in condition-wait) dump fully; an uninterruptible 99%-CPU spin that never
    ;; reaches a safepoint is reported as such instead of racing/truncating.
    (dolist (thread all-threads)
      (unless (eq thread current)
        (format stream "~&~%=== Thread: ~A ===~%" thread)
        (let ((sem (sb-thread:make-semaphore))
              (out (make-string-output-stream)))
          (handler-case
              (progn
                (sb-thread:interrupt-thread
                 thread
                 (lambda ()
                   (unwind-protect
                        (ignore-errors
                          (sb-debug:print-backtrace :stream out :print-thread nil :count 100))
                     (sb-thread:signal-semaphore sem))))
                (if (sb-thread:wait-on-semaphore sem :timeout 3)
                    (write-string (get-output-stream-string out) stream)
                    (format stream "  <no response in 3s: uninterruptible (99%-CPU spin, no safepoint)>~%")))
            (error (e) (format stream "  <could not backtrace: ~A>~%" e)))))
        (force-output stream)))
  ;; Dump fibers when sb-fiber is available
  #+sb-fiber
  (%dump-all-fiber-stacks :stream stream)
  (format stream "~&~80,,,'-@<~>~%")
  (format stream "End of thread dump~%")
  (format stream "~80,,,'-@<~>~%")
  (force-output stream)
  (values))

#+sb-fiber
(defun %dump-all-fiber-stacks (&key (stream *error-output*))
  "Dump state and backtraces for all fibers to STREAM."
  (let ((fibers (uiop:symbol-call :sb-thread :list-all-fibers)))
    (when fibers
      (format stream "~&~%~80,,,'=@<~>~%")
      (format stream "Fiber dump (~D fiber~:P)~%" (length fibers))
      (format stream "~80,,,'=@<~>~%")
      (dolist (fiber fibers)
        (let* ((state (uiop:symbol-call :sb-thread :fiber-state fiber))
               (name (uiop:symbol-call :sb-thread :fiber-name fiber))
               (java-thread (gethash fiber *fiber-to-java-threads*)))
          (format stream "~&~%=== Fiber: ~A  state: ~A ===~%"
                  (or name fiber) state)
          (when java-thread
            (format stream "  Java thread: ~A~%" java-thread))
          (case state
            ((:suspended :created)
             (ignore-errors
               (let ((bt (uiop:symbol-call :sb-thread :fiber-get-backtrace fiber)))
                 (if bt
                     (loop for frame in bt
                           for i from 0
                           do (format stream "  ~D: ~S~%" i frame))
                     (format stream "  <no backtrace available>~%")))))
            (:running
             (format stream "  <running on carrier — see thread dump above>~%"))
            (:dead
             (format stream "  <dead>~%"))))))))

(defun install-sigquit-handler ()
  "Install a SIGQUIT (signal 3) handler that dumps all thread stacks.
Send 'kill -3 <pid>' or 'kill -QUIT <pid>' to trigger."
  (sb-sys:enable-interrupt
   sb-unix:sigquit
   (lambda (sig info context)
     (declare (ignore sig info context))
     (%dump-all-thread-stacks :stream *error-output*)))
  (values))

(defvar *dump-trigger-file* "/tmp/openldk-dump-trigger"
  "When this file appears, the watchdog thread dumps all thread stacks and
deletes it.")

(defun install-dump-watchdog (&optional (trigger-file *dump-trigger-file*))
  "Spawn a watchdog thread that dumps all thread stacks (to *error-output*)
whenever TRIGGER-FILE appears, then deletes it.  A reliable alternative to the
SIGQUIT handler for diagnosing hangs -- because the watchdog is a separate,
always-live, interruptible thread, it fires even when the main thread is stuck
in a 99%-CPU spin or a 0%-CPU block (it reaches other threads via GC-safepoint
interrupts).  Usage on a hung process:  touch /tmp/openldk-dump-trigger"
  (let ((err *error-output*))
    (bordeaux-threads:make-thread
     (lambda ()
       (loop
         (sleep 0.5)
         (when (probe-file trigger-file)
           (ignore-errors (delete-file trigger-file))
           (ignore-errors (%dump-all-thread-stacks :stream err)))))
     :name "openldk-dump-watchdog"))
  (values))

(defun native-override-p (method)
  "True when METHOD should be satisfied by a native stub instead of generated bytecode."
  (let* ((class-slot (slot-value method 'class))
         (class-name (etypecase class-slot
                       (string class-slot)
                       (<class> (name class-slot)))))
    (or (and (string= class-name "java/lang/invoke/MethodHandleImpl")
             (member (slot-value method 'name)
                     '("makeArrays" "findCollector")
                     :test #'string=))
        ;; Javac: ClassReader$2 overrides setEnclosingType to throw; replace with
        ;; a native stub so we can delegate to our safe implementation.
        (and (string= class-name "com/sun/tools/javac/jvm/ClassReader$2")
             (string= (slot-value method 'name) "setEnclosingType")
             (string= (slot-value method 'descriptor) "(Lcom/sun/tools/javac/code/Type;)V")))))

(defun %find-branch-targets (code length)
  "Pre-scan bytecode to identify all PCs that are branch targets.
Returns a hash table mapping target PCs to T.  This is needed because
the main bytecode-to-IR pass processes instructions sequentially, so
backward branch targets would be incorrectly eliminated as dead code
if we relied solely on the stack-state-table (which is populated during
the forward pass)."
  (let ((targets (make-hash-table))
        (pc 0))
    (flet ((%read-s2 (base)
             (unsigned-to-signed-short
              (+ (ash (aref code base) 8) (aref code (1+ base)))))
           (%read-s4 (base)
             (unsigned-to-signed-integer
              (+ (ash (aref code base) 24) (ash (aref code (1+ base)) 16)
                 (ash (aref code (+ base 2)) 8) (aref code (+ base 3))))))
      (loop while (< pc length)
            for opcode = (aref code pc)
            do (cond
                 ;; Branch instructions with 2-byte signed offset:
                 ;; ifeq(99)..jsr(a8), ifnull(c6), ifnonnull(c7)
                 ((or (<= #x99 opcode #xa8)
                      (<= #xc6 opcode #xc7))
                  (setf (gethash (+ pc (%read-s2 (1+ pc))) targets) t)
                  (incf pc 3))
                 ;; goto_w(c8), jsr_w(c9) with 4-byte signed offset
                 ((or (= opcode #xc8) (= opcode #xc9))
                  (setf (gethash (+ pc (%read-s4 (1+ pc))) targets) t)
                  (incf pc 5))
                 ;; tableswitch - variable length
                 ((= opcode #xaa)
                  (let* ((aligned (logand (+ pc 4) (lognot 3))))
                    (setf (gethash (+ pc (%read-s4 aligned)) targets) t) ; default
                    (let* ((low (%read-s4 (+ aligned 4)))
                           (high (%read-s4 (+ aligned 8)))
                           (num-cases (1+ (- high low))))
                      (loop for i from 0 below num-cases
                            do (setf (gethash (+ pc (%read-s4 (+ aligned 12 (* i 4)))) targets) t))
                      (setf pc (+ aligned 12 (* num-cases 4))))))
                 ;; lookupswitch - variable length
                 ((= opcode #xab)
                  (let* ((aligned (logand (+ pc 4) (lognot 3))))
                    (setf (gethash (+ pc (%read-s4 aligned)) targets) t) ; default
                    (let ((npairs (%read-s4 (+ aligned 4))))
                      (loop for i from 0 below npairs
                            do (setf (gethash (+ pc (%read-s4 (+ aligned 8 (* i 8) 4))) targets) t))
                      (setf pc (+ aligned 8 (* npairs 8))))))
                 ;; wide prefix
                 ((= opcode #xc4)
                  (incf pc (if (= (aref code (1+ pc)) #x84) 6 4)))
                 ;; All other fixed-size instructions
                 (t
                  (incf pc (aref #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 00-0f
                                   2 3 2 3 3 2 2 2 2 2 1 1 1 1 1 1 ; 10-1f
                                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 20-2f
                                   1 1 1 1 1 1 2 2 2 2 2 1 1 1 1 1 ; 30-3f
                                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 40-4f
                                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 50-5f
                                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 60-6f
                                   1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 70-7f
                                   1 1 1 1 3 1 1 1 1 1 1 1 1 1 1 1 ; 80-8f
                                   1 1 1 1 1 1 1 1 1 3 3 3 3 3 3 3 ; 90-9f
                                   3 3 3 3 3 3 3 3 3 2 1 1 1 1 1 1 ; a0-af
                                   1 1 3 3 3 3 3 3 3 5 5 3 2 3 1 1 ; b0-bf
                                   3 3 1 1 1 4 3 3 5 5)             ; c0-c9
                                 opcode))))))
    targets))

(defun %eval (code)
  "Evaluate generated CODE, optionally printing and muffling warnings."
  (when *debug-codegen*
    (pprint code)
    (format t "~%"))
  (if *debug-unmuffle*
      (eval code) ; lint:suppress eval-usage
      (handler-bind
          ((style-warning (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'muffle-warning)))
           ;; Compiling a method replaces its stub.  When that happens while
           ;; COMPILE-FILE runs (a Lisp file whose compile-time code runs
           ;; Java), SBCL counts both DEFUNs as the file's and warns.
           (sb-int:duplicate-definition (lambda (c)
                                             (declare (ignore c))
                                             (invoke-restart 'muffle-warning))))
        (eval code)))) ; lint:suppress eval-usage

(defun lispize-method-name (name)
  "The Lisp symbol name for a Java method: NAME, its name and full
descriptor, return type included, as the JVM identifies a method (javac's
bridges and other compilers' methods differ only in their return types)."
  name)

(defun %resolve-method-name (class-name method-name)
  "METHOD-NAME, a Java method's name and argument descriptor with or without
its return type, completed with the return type of the method of that name
and arguments that CLASS-NAME (binary form) declares or inherits; or
METHOD-NAME itself when it has one already or none is found."
  (let ((close (position #\) method-name)))
    (if (or (null close) (< (1+ close) (length method-name)))
        method-name
        (labels ((search-class (name seen)
                   (let ((class (and name (not (member name seen :test #'string=))
                                     (ignore-errors (classload name)))))
                     (when class
                       (or (loop for m across (or (slot-value class 'methods) #())
                                 for full = (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))
                                 when (string= method-name (%short-method-name full))
                                   return full)
                           (search-class (slot-value class 'super) (cons name seen))
                           (loop for iface across (or (interfaces class) #())
                                 thereis (search-class iface (cons name seen))))))))
          (or (search-class class-name nil) method-name)))))

(defun %short-method-name (name)
  "NAME without its return type: how OpenLDK's hand-written natives were
named, and how Lisp code still names some of them."
  (let ((close (position #\) name)))
    (if close (subseq name 0 (1+ close)) name)))

(defun %compute-invoke-special-entry (gf owner-class nargs)
  "Compute the invoke-special cache entry for GF dispatching on OWNER-CLASS.
Returns a cons (method-fn . next-methods)."
  (let* ((class-list (cons owner-class
                           (make-list (max 0 (1- nargs))
                                      :initial-element (find-class 't))))
         (methods (if (typep gf 'java-generic-function)
                      (%java-applicable-methods gf owner-class class-list)
                      (closer-mop:compute-applicable-methods-using-classes gf class-list))))
    (unless methods
      (return-from %compute-invoke-special-entry nil))
    (let ((method (or (find owner-class methods
                            :key (lambda (m)
                                   (when (null (closer-mop::method-qualifiers m))
                                     (first (closer-mop:method-specializers m))))
                            :test #'eq :from-end t)
                      (find owner-class methods
                            :key (lambda (m)
                                   (first (closer-mop:method-specializers m)))
                            :test #'eq))))
      (unless method
        (setf method (first methods)))
      (let* ((tail (member method methods :test #'eq))
             (next (rest tail)))
        (cons (closer-mop:method-function method) next)))))

(defstruct (invoke-special-site (:constructor make-invoke-special-site
                                    (method-symbol owner-symbol)))
  "One invokespecial call site's cache of the entry INVOKE-SPECIAL finds:
valid while the generic function is GF and its invoke-special version is
VERSION."
  method-symbol owner-symbol (gf nil) (version -1 :type fixnum) (entry nil))

(defun invoke-special-at (site args)
  "INVOKE-SPECIAL of SITE's method and owner on ARGS, without looking the
generic function, owner class and entry up again while they're unchanged."
  (let ((gf (symbol-function (invoke-special-site-method-symbol site))))
    (if (and (eq gf (invoke-special-site-gf site))
             (eql (java-gf-invoke-special-version gf) (invoke-special-site-version site)))
        (let ((entry (invoke-special-site-entry site)))
          (funcall (car entry) args (cdr entry)))
        (let ((entry (and (typep gf 'java-generic-function)
                          ;; Read the version first: an invalidation after
                          ;; it leaves the site stale rather than wrong.
                          (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
                           (let* ((version (java-gf-invoke-special-version gf))
                                 (owner-class (find-class (invoke-special-site-owner-symbol site)))
                                 (entry (%compute-invoke-special-entry gf owner-class (length args))))
                            (when entry
                              ;; Recorded in GF's cache, whose invalidation
                              ;; bumps the version only for entries it holds.
                              (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
                                (setf (gethash owner-class (java-gf-invoke-special-cache gf)) entry))
                              ;; ENTRY before GF and VERSION, which validate it.
                              (setf (invoke-special-site-entry site) entry
                                    (invoke-special-site-gf site) nil
                                    (invoke-special-site-version site) version
                                    (invoke-special-site-gf site) gf))
                            entry)))))
          (if entry
              (funcall (car entry) args (cdr entry))
              (invoke-special (invoke-special-site-method-symbol site)
                              (invoke-special-site-owner-symbol site)
                              args))))))

(defvar *invoke-special-cache* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Plain generic function -> owner class -> (methods . entry), INVOKE-SPECIAL's
cache for generic functions that aren't JAVA-GENERIC-FUNCTIONs.")

(defun invoke-special (method-symbol owner-symbol args)
  "Invoke METHOD-SYMBOL on ARGS using Java invokespecial semantics.
OWNER-SYMBOL designates the declaring class of the target method.
This bypasses overriding methods on subclasses while still honouring
the normal call-next-method chain for the owner's superclasses."
  (let* ((gf (symbol-function method-symbol))
         (owner-class (find-class owner-symbol)))
    ;; Fast path: use invoke-special cache for java-generic-function GFs
    (when (typep gf 'java-generic-function)
      (let* ((cache (java-gf-invoke-special-cache gf))
             (entry (gethash owner-class cache)))
        (unless entry
          (bordeaux-threads:with-recursive-lock-held ((java-gf-method-lock gf))
            (setf entry (%compute-invoke-special-entry gf owner-class (length args)))
            (when entry
              (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
                (setf (gethash owner-class cache) entry)))))
        (when entry
          (return-from invoke-special
            (funcall (car entry) args (cdr entry))))))
    ;; A plain generic function (a hand-written native's): its entries are
    ;; cached with the method list they were computed from, which ADD-METHOD
    ;; and REMOVE-METHOD replace.  Looking every call up in full -- Kotlin's
    ;; compiler makes 17,000 such calls to String constructors in one
    ;; compilation -- took a tenth of its time.
    (when (typep gf 'standard-generic-function)
      (let* ((methods (closer-mop:generic-function-methods gf))
             (cache (or (gethash gf *invoke-special-cache*)
                        (setf (gethash gf *invoke-special-cache*)
                              (make-hash-table :test 'eq :synchronized t))))
             (cached (gethash owner-class cache))
             (entry (if (and cached (eq (car cached) methods))
                        (cdr cached)
                        (let ((entry (%compute-invoke-special-entry gf owner-class (length args))))
                          (when entry
                            (setf (gethash owner-class cache) (cons methods entry)))
                          entry))))
        (when entry
          (return-from invoke-special
            (funcall (car entry) args (cdr entry))))))
    ;; Slow path: full MOP lookup for non-java-generic-function GFs
    (let* ((class-list (cons owner-class
                             (loop repeat (max 0 (1- (length args)))
                                   collect (find-class 't))))
           (methods (closer-mop:compute-applicable-methods-using-classes gf class-list)))
      (unless methods
        (internal-error "No applicable methods found for ~A on declaring class ~A with ~D args"
               method-symbol owner-symbol (length args)))
      (let ((method (or (find owner-class methods
                              :key (lambda (m)
                                     (when (null (closer-mop::method-qualifiers m))
                                       (first (closer-mop:method-specializers m))))
                              :test #'eq :from-end t)
                       (find owner-class methods
                             :key (lambda (m)
                                    (first (closer-mop:method-specializers m)))
                             :test #'eq))))
        (unless method
          (setf method (first methods)))
        (let* ((tail (member method methods :test #'eq))
               (next (rest tail)))
          (funcall (closer-mop:method-function method) args next))))))
(defun make-exception-handler-table (context)
  "Build a hashtable of handler PCs keyed by handler start for CONTEXT."
  (let ((exception-table (exception-table context))
        (exception-handler-table (make-hash-table)))
    (when exception-table
      (loop for i from 0 below (length exception-table)
            for ete = (aref exception-table i)
            do (setf (gethash (handler-pc ete) exception-handler-table) t)))
    exception-handler-table))

(defun %method-fn-name (class method)
  "Lisp function name for METHOD: \"Class.name(desc)\" for static methods,
\"name(desc)\" for instance methods (which dispatch on a shared GF)."
  (let ((mangled (lispize-method-name
                  (format nil "~A~A"
                          (slot-value method 'name)
                          (slot-value method 'descriptor)))))
    (if (static-p method)
        (format nil "~A.~A" (slot-value class 'name) mangled)
        mangled)))

(defun %class-loader-package (class)
  "Package for CLASS's loader-scoped function symbols, defaulting to :openldk."
  (if (slot-value class 'ldk-loader)
      (loader-package (slot-value class 'ldk-loader))
      (find-package :openldk)))

(defun %class-specializer-package (class)
  "Package holding CLASS's CLOS class symbol (per-loader, or OPENLDK.SYSTEM)."
  (if (slot-value class 'ldk-loader)
      (loader-package (slot-value class 'ldk-loader))
      (or (find-package "OPENLDK.SYSTEM")
          (find-package :openldk))))

(defun %install-native-override (class method method-key)
  "If METHOD-KEY has a native override, install it in place of the stub and
return T.  Instance methods are installed as CLOS methods so other classes'
methods on the same generic function survive."
  (when-let ((native-fn (gethash method-key *native-overrides*)))
    (let ((fn-symbol (intern (%method-fn-name class method)
                             (%class-loader-package class)))
          (owned-p (%user-ldk-loader-p (ldk-loader class))))
      (if (static-p method)
          (if owned-p
              (%install-owned-java-function class fn-symbol native-fn (count-parameters (descriptor method)))
              (setf (symbol-function fn-symbol) native-fn))
          (let* ((class-sym (intern (slot-value class 'name)
                                    (%class-specializer-package class)))
                 (n-params (progn
                             (%ensure-java-gf fn-symbol (1+ (count-parameters (slot-value method 'descriptor))))
                             (count-parameters (slot-value method 'descriptor))))
                 (param-names (loop for i from 1 upto n-params
                                    collect (intern (format nil "arg~A" i) :openldk)))
                 (this-sym (intern "this" :openldk))
                 (entry (and owned-p (intern (format nil "%jnative:~A" method-key) (%class-loader-package class)))))
            (when entry (%install-owned-java-function class entry native-fn (1+ n-params)))
            (%eval `(defmethod ,fn-symbol
                        ((,this-sym ,class-sym) ,@param-names)
                      (funcall ,(if entry `(function ,entry) native-fn) ,this-sym ,@param-names))))))
    (setf (gethash method-key *methods-being-compiled*) :done)
    t))

(defun %claim-method-compilation (method-key)
  "Atomically claim METHOD-KEY for compilation by this thread.  Return T if
the caller should compile it, NIL if it is already compiled (or, in AOT mode,
being compiled elsewhere).  Blocks while another thread is compiling it.
A claim records the compiling thread, since class resolution can run Java
code mid-compile."
  (bt:with-lock-held (*method-compilation-lock*)
    (loop
      (let ((status (gethash method-key *methods-being-compiled*)))
        (cond
          ((eq status :done)
           ;; Already compiled by another thread
           (return nil))
          ((eq status (bt:current-thread))
           ;; This thread is already compiling it, further up the stack:
           ;; waiting would wait on ourselves forever.
           (if *aot-dir*
               (return nil)
               (internal-error "recursive compilation of ~A" method-key)))
          (status
           ;; In AOT mode, don't wait - just skip methods being compiled to
           ;; avoid recursion; in normal mode, wait for the other thread.
           (if *aot-dir*
               (return nil)
               (bt:condition-wait *method-compilation-cv* *method-compilation-lock*)))
          (t
           (setf (gethash method-key *methods-being-compiled*) (bt:current-thread))
           (return t)))))))

(defvar *stack-hints* nil
  "PC -> the operand stack a previous pass recorded there, for a PC only a
later (backward) branch reaches; see %TRANSPILE-WITH-STACK-HINTS.")

(defun %hinted-stack (hint pc)
  "A stack shaped like HINT, a recorded stack, of fresh stack variables."
  (append (mapcar (lambda (var) (make-stack-variable *context* pc (slot-value var 'var-type)))
                  (butlast hint))
          (list (make-instance '<stack-bottom-marker>))))

(defun %transpile-with-stack-hints (method-key code length)
  "Transpile, as %TRANSPILE-METHOD-BYTECODE, once more if needed.  The
single forward pass reaches an instruction after an unconditional branch
with an empty stack when only a later backward branch leads to it, and
javac never leaves values on the stack across one -- but other compilers
do (Kawa keeps an array there while filling it).  If that instruction turns
out to need a deeper stack, transpile again in a fresh context, starting it
with fresh variables shaped like the stack recorded there."
  (let ((result (catch '%retry-transpile
                  (list :done (%transpile-method-bytecode method-key code length)))))
    (if (and (consp result) (eq (first result) :done))
        (second result)
        (let ((template *context*))
          (setf *context* (make-instance '<context>
                                         :class (slot-value template 'class)
                                         :ldk-loader (slot-value template 'ldk-loader)
                                         :classes (slot-value template 'classes)
                                         :exception-table (slot-value template 'exception-table)
                                         :bytecode code
                                         :insn-size (make-array length :element-type 'fixnum :initial-element -1)
                                         :next-insn-list (make-array length :initial-element nil)
                                         :stack-state-table (make-hash-table)
                                         :pc 0
                                         :is-clinit-p (slot-value template 'is-clinit-p)))
          (setf (svcount *context*) 0
                (fn-name *context*) (slot-value template 'fn-name))
          (let ((*stack-hints* result))
            (%transpile-method-bytecode method-key code length))))))

(defun %transpile-method-bytecode (method-key code length)
  "Transpile METHOD-KEY's bytecode CODE into IR: run the per-opcode
transpilers, tracking reachability and per-PC stack state in *CONTEXT*;
then unify stack variables across control-flow joins and batch array
initializations.  Returns the IR instruction list."
  (let* ((exception-handler-table (make-exception-handler-table *context*))
         (branch-targets (%find-branch-targets code length))
         (in-dead-code nil) ;; Track unreachable code after unconditional branches
         (after-jump nil)   ;; The previous instruction was an unconditional branch
         (guessed-pcs '())  ;; Branch targets entered with an empty stack after one
         (ir-code
           (apply #'append
                  (loop
                    while (< (pc *context*) length)
                    for no-record-stack-state? = (find (aref *opcodes*
                                                            (aref code (pc *context*)))
                                                       '(:GOTO :ATHROW :RETURN :IRETURN
                                                         :LRETURN :FRETURN :DRETURN :ARETURN
                                                         :TABLESWITCH :LOOKUPSWITCH))
                    for was-in-dead-code = in-dead-code
                    for result = (progn
                                   ;; Check if we're at a branch target - exit dead code mode
                                   (let ((stk (gethash (pc *context*) (stack-state-table *context*))))
                                     (when stk
                                       ;; Normally the latest recorded stack.
                                       ;; After a guess, the earliest: later
                                       ;; ones may descend from the guess.
                                       (setf (stack *context*) (if guessed-pcs
                                                                   (car (last stk))
                                                                   (car stk)))
                                       (setf in-dead-code nil)
                                       (setf (in-dead-code *context*) nil)
                                       (setf was-in-dead-code nil))
                                     ;; A branch target after an unconditional
                                     ;; branch, reached only by a later branch:
                                     ;; use the stack a previous pass recorded,
                                     ;; or note that the empty one is a guess.
                                     (when (and (null stk) after-jump
                                                (gethash (pc *context*) branch-targets))
                                       (let ((hint (and *stack-hints*
                                                        (gethash (pc *context*) *stack-hints*))))
                                         (if hint
                                             (setf (stack *context*) (%hinted-stack (car hint) (pc *context*)))
                                             (push (pc *context*) guessed-pcs)))))
                                   ;; Check if we're at an exception handler - exit dead code mode
                                   (let ((pc-start (pc *context*)))
                                     (when (gethash pc-start exception-handler-table)
                                       (setf in-dead-code nil)
                                       (setf (in-dead-code *context*) nil)
                                       (setf was-in-dead-code nil)))
                                   (when (and *debug-bytecode* (not was-in-dead-code))
                                     (format t "~&; ~A c[~A] ~A ~@<~A~:@>"
                                             method-key
                                             (pc *context*)
                                             (aref *opcodes* (aref code (pc *context*)))
                                             (stack *context*)))
                                   ;; Always call transpiler to populate insn-size and next-insn-list
                                   (let* ((pc-start (pc *context*)))
                                     (if (and (gethash pc-start exception-handler-table)
                                              (not was-in-dead-code))
                                         ;; Normally the handler's stack is just the
                                         ;; exception.  If normal code also reaches the
                                         ;; handler, with one value on its stack (Kawa's
                                         ;; try-finally passes null), share that value's
                                         ;; variable and take the exception only on entry
                                         ;; from a HANDLER-CASE clause.
                                         (let* ((normal (and (gethash pc-start (stack-state-table *context*))
                                                             (typep (first (stack *context*)) '<stack-variable>)
                                                             (typep (second (stack *context*)) '<stack-bottom-marker>)
                                                             (first (stack *context*))))
                                                (var (or normal (make-stack-variable *context* pc-start :REFERENCE))))
                                           (cond
                                             (normal
                                              (setf (gethash pc-start (shared-handler-table *context*)) t)
                                              ;; Assigned here too: not single-assignment,
                                              ;; so copy propagation leaves it alone.
                                              (push (incf (svcount *context*)) (var-numbers normal)))
                                             ;; The handler's stack is just the exception,
                                             ;; whatever the code before it left.
                                             (t (setf (stack *context*)
                                                      (list var (make-instance '<stack-bottom-marker>)))))
                                           (cons (make-instance 'ir-assign
                                                                :address pc-start
                                                                :lvalue var
                                                                :rvalue (make-instance 'ir-condition-exception
                                                                                       :normal-value normal))
                                                 (mapcar (lambda (insn)
                                                           (with-slots (address) insn
                                                             (setf address (+ address 0.1)))
                                                           insn)
                                                         (funcall
                                                          (aref *opcodes* (aref code (pc *context*)))
                                                          *context* code))))
                                         (funcall
                                          (aref *opcodes* (aref code (pc *context*)))
                                          *context* code))))
                    ;; Enter dead code mode after unconditional branches,
                    ;; but NOT if the next instruction is a branch target
                    ;; (it may be reachable via a backward branch that
                    ;; hasn't been processed yet in this forward pass).
                    do (setf after-jump no-record-stack-state?)
                    when no-record-stack-state?
                      do (unless (gethash (pc *context*) branch-targets)
                           (setf in-dead-code t)
                           (setf (in-dead-code *context*) t))
                    unless (or was-in-dead-code no-record-stack-state?)
                      do (%record-stack-state (pc *context*) *context*)
                    unless (or (null result) was-in-dead-code)
                      collect result))))
    ;; A guessed (empty) stack was wrong if a branch later recorded a deeper
    ;; one there: start over with what was recorded.
    (when (and (null *stack-hints*)
               (some (lambda (pc)
                       (some (lambda (stack) (> (length stack) 1))
                             (gethash pc (stack-state-table *context*))))
                     guessed-pcs))
      (throw '%retry-transpile (stack-state-table *context*)))
    ;; Do stack analysis to merge stack variables
    ;; When multiple control flow paths reach the same PC, we need to
    ;; unify the stack variables. merge-stacks has side effects - it
    ;; mutates the var-numbers slot of stack variables to include the
    ;; union of all paths. The return value is discarded; the important
    ;; work is the mutation of shared stack-variable objects.
    (handler-bind
        ((error (lambda (e)
                  (format *error-output* "~&Error in method ~A: ~A~%"
                          method-key e))))
      (maphash (lambda (k v)
                 (when (> (length v) 1)
                   (reduce (lambda (list1 list2) (merge-stacks list1 list2 k)) v)))
               (stack-state-table *context*)))
    (fix-stack-variables (stack-variables *context*))
    (loop
      (multiple-value-bind (new-code changed?)
          (initialize-arrays ir-code *context*)
        (unless changed?
          (return))
        (setf ir-code new-code)))
    ir-code))

(defun %optimize-method-blocks (ir-code)
  "Build basic blocks from IR-CODE and run the optimizer passes selected by
the *ENABLE-* flags: reaching definitions, copy propagation, and dead-store
elimination.  Returns the optimized block list."
  (let* ((blocks-before-prop (build-basic-blocks ir-code))
         ;; Phase 3: Compute reaching definitions for inter-block propagation
         (reaching-in (when (and *enable-copy-propagation*
                                 *enable-reaching-definitions*)
                        (reaching-definitions-fixpoint blocks-before-prop)))
         ;; Per-block propagation with separate scopes for locals vs globals
         (blocks (if *enable-copy-propagation*
                     (let ((global-table (single-assignment-table *context*)))
                       ;; Phase 3: Apply reaching definitions before per-block propagation
                       (when *enable-reaching-definitions*
                         (dolist (block blocks-before-prop)
                           (apply-reaching-definitions block reaching-in global-table)))
                       ;; Phase 2: Per-block local propagation
                       (mapcar (lambda (block)
                                 (let ((block-code (slot-value block 'code))
                                       ;; Per-block table for local variables only
                                       (block-local-table (when *enable-local-propagation*
                                                            (make-hash-table :test #'eq))))
                                   (setf (slot-value block 'code)
                                         (propagate-copies block-code global-table
                                                           :allow-locals *enable-local-propagation*
                                                           :local-table block-local-table))
                                   block))
                               blocks-before-prop))
                     blocks-before-prop)))
    ;; Dead code elimination: remove assignments to stack vars that are never read
    (when *enable-dce*
      (loop while (eliminate-dead-stack-assignments blocks)))
    blocks))

(defun %wrap-method-body (static-p fn-sym impl-sym class-spec args body-forms)
  "Build the installable definition form for a compiled method body.
Static methods become a DEFUN.  Instance methods put BODY-FORMS in a
plain DEFUN on IMPL-SYM and install a method calling it: a closure (see
%INSTALL-TRAMPOLINE), or for a plain generic function a one-line
DEFMETHOD.  PCL's DEFMETHOD expansion walks the entire body (sb-walker)
looking for call-next-method, which is quadratic on large generated
bodies and pure waste since compiled Java methods never use it."
  (let ((this-sym (intern "this" :openldk)))
    (if static-p
        `(defun ,fn-sym ,args ,@body-forms)
        `(progn
           (defun ,impl-sym (,this-sym ,@args)
             ;; Generated code returns via (return-from <method-name> ...),
             ;; matching the block DEFMETHOD used to establish.
             (block ,fn-sym ,@body-forms))
           ,@(if (typep (and (fboundp fn-sym) (fdefinition fn-sym)) 'java-generic-function)
                 ;; It calls the body directly (see
                 ;; %COMPUTE-JAVA-EFFECTIVE-METHOD), so the trampoline
                 ;; needn't be fast.
                 `((%install-trampoline ',fn-sym ',class-spec ',impl-sym ,(length args)))
                 ;; A plain generic function -- a hand-written native's, as
                 ;; hashCode's is -- calls the method's function, and PCL
                 ;; calls a compiled DEFMETHOD's fastest.
                 `((defmethod ,fn-sym ((,this-sym ,class-spec) ,@args)
                     (,impl-sym ,this-sym ,@args))
                   (%note-method-body ',fn-sym ',class-spec ',impl-sym)))))))

;;; Declaring method variables' types (see the note on typed variables in
;;; codegen.lisp).

(defun %variable-categories (sources)
  "Map each variable in SOURCES (a hash table of variable -> its value
sources, see %NOTE-VARIABLE-SOURCE) to the one category all its values
have, omitting variables with mixed or unknown values.  Copies resolve
optimistically, so a loop that copies a variable around stays typed."
  (let ((categories (make-hash-table :test #'eq))
        (rejected (make-hash-table :test #'eq)))
    (flet ((reject (variable)
             (remhash variable categories)
             (setf (gethash variable rejected) t)))
      (loop for variable being the hash-keys of sources using (hash-value list)
            for direct = (remove-if #'consp list)
            do (cond
                 ((or (member nil list)
                      (notevery (lambda (c) (eq c (first direct))) direct))
                  (reject variable))
                 (direct (setf (gethash variable categories) (first direct)))))
      ;; Settle copies: a copy needs its source typed alike.  Each pass only
      ;; rejects variables or types untyped ones, so this terminates.
      (loop
        (let ((changed nil))
          (loop for variable being the hash-keys of sources using (hash-value list)
                unless (gethash variable rejected)
                  do (dolist (source list)
                       (when (consp source)
                         (let ((from (cdr source)))
                           (cond
                             ((or (gethash from rejected)
                                  (not (nth-value 1 (gethash from sources))))
                              (reject variable) (setf changed t) (return))
                             ((gethash from categories)
                              (let ((mine (gethash variable categories)))
                                (cond
                                  ((null mine)
                                   (setf (gethash variable categories) (gethash from categories)
                                         changed t))
                                  ((not (eq mine (gethash from categories)))
                                   (reject variable) (setf changed t) (return))))))))))
          (unless changed (return))))
      ;; Copies from a variable that never got typed (a cycle of copies
      ;; holding no value) can't be trusted either.
      (loop while (loop for variable being the hash-keys of categories
                        when (some (lambda (source)
                                     (and (consp source)
                                          (not (gethash (cdr source) categories))))
                                   (gethash variable sources))
                          do (reject variable)
                          and return t))
      categories)))

(defun %type-normalizer (lisp-type)
  (cond ((equal lisp-type '(signed-byte 32)) '%as-int)
        ((equal lisp-type '(signed-byte 64)) '%as-long)
        ((eq lisp-type 'single-float) '%as-float)
        (t '%as-double)))

(defun %normalize-typed-assignments (form types)
  "Rewrite each (SETF VAR VALUE) in FORM whose VAR has a declared type in
the alist TYPES so VALUE passes through that type's normalizer."
  (cond
    ((atom form) form)
    ((eq (car form) 'quote) form)
    ((and (eq (car form) 'setf)
          (consp (cdr form)) (consp (cddr form)) (null (cdddr form))
          (symbolp (second form))
          (assoc (second form) types))
     `(setf ,(second form)
            (,(%type-normalizer (cdr (assoc (second form) types)))
             ,(%normalize-typed-assignments (third form) types))))
    (t (cons (%normalize-typed-assignments (car form) types)
             (%normalize-typed-assignments (cdr form) types)))))

(defun %typed-let (bindings body)
  "The LET binding a method's BINDINGS around BODY, declaring the type of
every variable whose values all have one primitive category."
  (when (compact-p *context*)
    (return-from %typed-let `(let ,bindings ,@body)))
  (let ((sources (variable-sources *context*)))
    ;; THIS and the parameters' locals start out holding values of unknown
    ;; type: callers pass whatever Java fields hold.
    (loop for (symbol init) in bindings
          when init do (%note-variable-source symbol nil *context*))
    (let* ((categories (%variable-categories sources))
           (types (loop for (symbol) in bindings
                        for category = (gethash symbol categories)
                        when category
                          collect (cons symbol (ecase category
                                                 (:int '(signed-byte 32))
                                                 (:long '(signed-byte 64))
                                                 (:float 'single-float)
                                                 (:double 'double-float))))))
      (if (null types)
          `(let ,bindings ,@body)
          `(let ,(loop for (symbol init) in bindings
                       for type = (cdr (assoc symbol types))
                       collect (cond
                                 ((null type) (if init (list symbol init) (list symbol)))
                                 (t (list symbol (if (consp type) 0 (coerce 0 type))))))
             (declare ,@(loop for (symbol . type) in types
                              collect `(type ,type ,symbol)))
             ,@(%normalize-typed-assignments body types))))))

(defun %prune-unused-bindings (bindings body)
  "BINDINGS less those with no initial value that BODY never mentions."
  (let ((used (make-hash-table :test #'eq)))
    (labels ((walk (form)
               (cond ((symbolp form) (setf (gethash form used) t))
                     ((consp form) (walk (car form)) (walk (cdr form))))))
      (walk body))
    (remove-if (lambda (binding)
                 (and (null (rest binding))
                      (not (gethash (first binding) used))))
               bindings)))

(defun %method-let (bindings body)
  "The LET binding a compiled method's variables around BODY.  BINDINGS
covers every local slot, stack variable and exception-handler variable
the method might use; those with no initial value that BODY never
mentions are dropped, and %TYPED-LET declares the types of the rest."
  (%typed-let (%prune-unused-bindings bindings body) body))

;;; Folding single-use stack variables.  Bytecode passes nearly every
;;; value through a stack variable: (setf s{5} (call ...)) followed by a
;;; statement that reads s{5} once.  When nothing in that statement is
;;; evaluated before the read except variables and constants, putting the
;;; expression in place of the read keeps every side effect in the same
;;; order, and leaves SBCL a nested expression instead of an assigned
;;; variable.  This works on the generated Lisp, where evaluation order is
;;; plain left to right, and only through forms whose order it knows.

(defparameter *pure-functions*
  '(+ - * = /= < > <= >= eq eql not null logand logior logxor lognot ash
    sb-c::mask-signed-field ldb float coerce)
  "Functions with no side effects that generated code calls.")

(defun %stack-variable-symbol-p (x)
  (and (symbolp x)
       (eq (symbol-package x) (find-package :openldk))
       (let ((name (symbol-name x)))
         (and (> (length name) 2) (char= (char name 0) #\s) (char= (char name 1) #\{)))))

(defun %fold-scan (form variable)
  "How evaluating FORM reaches VARIABLE: :FOUND when VARIABLE is evaluated
first and unconditionally, with only side-effect-free forms before it;
:PURE when FORM has no side effects and doesn't mention VARIABLE;
otherwise :BLOCKED."
  (labels ((scan-sequence (forms)
             (dolist (f forms :pure)
               (let ((r (%fold-scan f variable)))
                 (unless (eq r :pure) (return r))))))
    (cond
      ((eq form variable) :found)
      ((atom form) :pure)
      (t
       (let ((head (car form)))
         (case head
           ((quote function load-time-value) :pure)
           (setf
            (let ((place (second form)))
              ;; Places are evaluated left to right, then the value; the
              ;; assignment itself is a side effect.
              (let ((r (cond ((/= (length form) 3) :blocked)
                             ((symbolp place) (scan-sequence (cddr form)))
                             ((and (consp place)
                                   (member (car place)
                                           '(aref jaref %cached-slot-value %java-slot-value)))
                              (scan-sequence (append (cdr place) (cddr form))))
                             (t :blocked))))
                (if (eq r :pure) :blocked r))))
           ((let let*)
            (scan-sequence (append (mapcar (lambda (b) (if (consp b) (second b) nil))
                                           (second form))
                                   (cddr form))))
           (progn (scan-sequence (cdr form)))
           (return-from (if (eq (%fold-scan (third form) variable) :found) :found :blocked))
           ((the sb-ext:truly-the) (%fold-scan (third form) variable))
           ((if when unless)
            (if (eq (%fold-scan (second form) variable) :found) :found :blocked))
           (t
            (if (and (symbolp head)
                     (not (special-operator-p head))
                     (not (macro-function head)))
                ;; A function call: its arguments, in order, then the call.
                (let ((r (scan-sequence (cdr form))))
                  (if (and (eq r :pure) (not (member head *pure-functions*)))
                      :blocked
                      r))
                :blocked))))))))

(defun %count-variable-references (form counts)
  (cond ((%stack-variable-symbol-p form) (incf (gethash form counts 0)))
        ((atom form))
        ((eq (car form) 'quote))
        (t (loop for tail on form
                 do (%count-variable-references (car tail) counts)
                 while (consp (cdr tail))))))

(defun %fold-statements (statements counts categories)
  "Fold each (SETF s{N} E) in the tagbody STATEMENTS into the statement
after it, as described above.  When CATEGORIES (see %VARIABLE-CATEGORIES)
gives s{N} a primitive category, E goes in wrapped in that category's
normalizer, keeping the type SBCL would have had from the declaration of
s{N}: without it a folded array length compares by generic arithmetic."
  (let ((out '()))
    (dolist (statement statements (nreverse out))
      (unless (atom statement)
        (loop for previous = (first out)
              while (and (consp previous)
                         (eq (first previous) 'setf)
                         (= (length previous) 3)
                         (%stack-variable-symbol-p (second previous))
                         (= (gethash (second previous) counts 0) 2)
                         (eq (%fold-scan statement (second previous)) :found))
              do (let* ((variable (second previous))
                        (category (and categories (gethash variable categories)))
                        (value (if category
                                   (list (%category-normalizer category) (third previous))
                                   (third previous))))
                   (setf statement (subst value variable statement))
                   (pop out))))
      (push statement out))))

(defun %category-normalizer (category)
  (ecase category
    (:int '%as-int)
    (:long '%as-long)
    (:float '%as-float)
    (:double '%as-double)))

(defun %fold-stack-variables (form categories
                              &optional (counts (let ((c (make-hash-table :test #'eq)))
                                                  (%count-variable-references form c)
                                                  c)))
  "FORM, a method body, with single-use stack variables folded into their
use (see %FOLD-STATEMENTS for CATEGORIES)."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        (t (let ((form (loop for tail on form
                             collect (%fold-stack-variables (car tail) categories counts)
                               into items
                             unless (listp (cdr tail))
                               do (return (nconc items (cdr tail)))
                             finally (return items))))
             (if (eq (car form) 'tagbody)
                 (cons 'tagbody (%fold-statements (cdr form) counts categories))
                 form)))))

;;; Cleanups of the generated method body that need no flow analysis.

(defun %go-targets (form &optional (targets (make-hash-table :test #'eq)))
  "Hash set of the tags FORM's GO forms jump to."
  (cond ((atom form))
        ((eq (car form) 'quote))
        ((and (eq (car form) 'go) (consp (cdr form)))
         (setf (gethash (second form) targets) t))
        (t (loop for tail on form
                 do (%go-targets (car tail) targets)
                 while (consp (cdr tail)))))
  targets)

(defun %class-init-check-class (statement)
  "The class STATEMENT, a (UNLESS (INITIALIZED-P class) (%clinit-X)) check,
initializes, or NIL."
  (and (consp statement)
       (eq (first statement) 'unless)
       (= (length statement) 3)
       (consp (second statement))
       (eq (first (second statement)) 'initialized-p)
       (second (second statement))))

(defun %clean-statements (statements targets)
  "Tagbody STATEMENTS without the tags nothing jumps to, and without class
init checks repeating one made earlier in the same straight-line run."
  (let ((checked '())
        (out '()))
    (dolist (statement statements (nreverse out))
      (cond
        ((atom statement)
         (when (gethash statement targets)
           ;; Control can arrive here from elsewhere.
           (setf checked '())
           (push statement out)))
        ((let ((class (%class-init-check-class statement)))
           (and class (member class checked :test #'eq))))
        (t
         (let ((class (%class-init-check-class statement)))
           (when class (push class checked)))
         (push statement out))))))

(defun %clean-method-body (form targets this-p)
  "FORM, a method body, with unused tags and repeated class init checks
removed, and when THIS-P, (%RECEIVER local-0) as plain local-0: in an
instance method that never assigns local-0 it is THIS, which dispatch
already made a non-null Java object."
  (cond ((atom form) form)
        ((eq (car form) 'quote) form)
        ((and this-p
              (eq (car form) '%receiver)
              (eq (second form) (intern "local-0" :openldk)))
         (second form))
        (t (let ((form (loop for tail on form
                             collect (%clean-method-body (car tail) targets this-p) into items
                             unless (listp (cdr tail))
                               do (return (nconc items (cdr tail)))
                             finally (return items))))
             (if (eq (car form) 'tagbody)
                 (cons 'tagbody (%clean-statements (cdr form) targets))
                 form)))))

(defun %assigns-p (form variable)
  "True when FORM contains (SETF VARIABLE ...)."
  (cond ((atom form) nil)
        ((eq (car form) 'quote) nil)
        ((and (eq (car form) 'setf) (eq (second form) variable)) t)
        (t (loop for tail on form
                 thereis (%assigns-p (car tail) variable)
                 while (consp (cdr tail))))))

(defun %codegen-method-blocks (blocks)
  "The statements of a method's TAGBODY: each of BLOCKS' code."
  (mapcan (lambda (x) (if (listp x) x (list x)))
          (loop for block in blocks
                for code = (codegen-block block block)
                when code
                  collect code)))

(defun %flat-try-body (statements regions)
  "STATEMENTS, a method's flat TAGBODY (see *FLAT-TRY-REGIONS*), wrapped in
the one handler that dispatches on %REGION: a Java throwable signalled in a
region that catches it is kept in CONDITION-CACHE, unwinds to %ATTEMPT,
and re-enters the TAGBODY through a jump table to its handler.  Locals
live outside, so they keep their values, as the JVM's do.  Returns NIL
when STATEMENTS can't be flat: some tag twice, or a handler not emitted."
  (let ((tags (remove-if-not #'symbolp statements))
        (handlers (make-hash-table)))
    (unless (= (length tags) (length (remove-duplicates tags)))
      (return-from %flat-try-body nil))
    (maphash (lambda (entries region)
               (declare (ignore region))
               (loop for (nil . handler-pc) in entries
                     for block = (gethash handler-pc (block-address-table *context*))
                     for tag = (and block (intern (format nil "branch-target-~A" (address block))))
                     unless (and tag (member tag tags))
                       do (return-from %flat-try-body nil)
                     do (setf (gethash handler-pc handlers) block)))
             regions)
    (let ((condition (intern "%condition" :openldk)))
      `((let ((%region -1) (%resume -1))
          (declare (fixnum %region %resume))
          (tagbody
           %retry
             (block %attempt
               (handler-bind
                   ((|condition-java/lang/Throwable|
                      (lambda (,condition)
                        (let ((handler
                                (case %region
                                  ,@(loop for entries being the hash-keys of regions using (hash-value region)
                                          collect `(,region
                                                    (typecase ,condition
                                                      ,@(loop for (catch-type . handler-pc) in entries
                                                              when (> (length catch-type) 0)
                                                                do (%resolve-class-in-context catch-type)
                                                              collect `(,(intern (format nil "condition-~A"
                                                                                         (if (> (length catch-type) 0)
                                                                                             catch-type
                                                                                             "java/lang/Throwable"))
                                                                                 :openldk)
                                                                        ,handler-pc)))))
                                  (t nil))))
                          (when handler
                            (when *debug-exceptions*
                              (format *error-output* "~&Caught Java exception in ~A.~A~%"
                                      ,(slot-value (slot-value *context* 'class) 'name)
                                      ,(fn-name *context*))
                              (%print-java-stack-trace (slot-value ,condition '|objref|)
                                                       :stream *error-output*))
                            (setf |condition-cache| ,condition
                                  %resume handler)
                            (return-from %attempt))))))
                 (openldk::with-java-conditions
                     (:arrays ,(and (needs-array-bounds-check *context*) t))
                   (tagbody
                      (case %resume
                        ,@(loop for handler-pc being the hash-keys of handlers using (hash-value block)
                                collect `(,handler-pc
                                          (setf %resume -1)
                                          ,@(%handler-entry-flag block)
                                          (go ,(intern (format nil "branch-target-~A" (address block)))))))
                      ,@statements)))
               (go %done))
             (go %retry)
           %done))))))

(defun %build-method-definition (class method blocks-after-dce parameter-hints max-locals)
  "Build the complete DEFUN/DEFMETHOD form for METHOD from its optimized
BLOCKS-AFTER-DCE: generate each block's code, then wrap it with debug
tracing, the NPE/array-bounds handler-binds, and ACC_SYNCHRONIZED monitor
entry/exit as required."
  (let* ((lisp-code
           (%fold-stack-variables
            (list (list* 'block nil
                         (or (and (plusp (length (exception-table *context*)))
                                  (let ((*flat-try-regions* (make-hash-table :test #'equal)))
                                    (%flat-try-body (%codegen-method-blocks blocks-after-dce)
                                                    *flat-try-regions*)))
                             ;; Nested HANDLER-CASEs, as before flat regions.
                             (progn
                               (setf (emitted-block-scopes *context*) (list (list)))
                               (list (cons 'tagbody (%codegen-method-blocks blocks-after-dce)))))))
            (unless (compact-p *context*)
              (%variable-categories (variable-sources *context*)))))
         (lisp-code
           (%clean-method-body lisp-code
                               (%go-targets lisp-code)
                               (and (not (static-p method))
                                    (not (%assigns-p lisp-code (intern "local-0" :openldk))))))
         (traced-lisp-code (if *debug-trace* `((unwind-protect
                                                    ,(car lisp-code)
                                                 (incf *call-nesting-level* -1)))
                               lisp-code))
         ;; Always convert null pointer dereferences (SIGSEGV at NIL+offset)
         ;; and Lisp type-errors to Java NullPointerException.  Array bounds
         ;; conversion is only needed when the method contains array
         ;; operations.  This method-level wrapper catches what escapes the
         ;; method's try blocks; each protected form establishes the same
         ;; conversion inside itself (codegen.lisp) so that a throwable
         ;; manufactured here is catchable by the catch clauses guarding it.
         (null-checked-lisp-code
           `((openldk::with-java-conditions
                 (:arrays ,(and (needs-array-bounds-check *context*) t))
               ,(car traced-lisp-code))))
         ;; For ACC_SYNCHRONIZED methods, wrap body with monitor-enter/exit.
         ;; Instance methods synchronize on 'this'; static methods on the Class object.
         (synchronized-lisp-code
           (if (synchronized-p method)
               (let ((monitor-obj
                       (if (static-p method)
                           `(openldk::|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;|
                             (openldk::jstring ,(slot-value class 'name)) nil nil nil)
                           (intern "this" :openldk))))
                 `((let ((%sync-monitor-obj% ,monitor-obj))
                     (monitor-enter %sync-monitor-obj%)
                     (unwind-protect
                          ,(car null-checked-lisp-code)
                       (monitor-exit %sync-monitor-obj%)))))
               null-checked-lisp-code)))
    (let ((parameter-count (count-parameters (slot-value method 'descriptor))))
      (let ((args (if (static-p method)
                      (loop for i from 1 upto parameter-count
                            collect (intern (format nil "arg~A" (1- i)) :openldk))
                      (loop for i from 1 upto parameter-count
                            collect (intern (format nil "arg~A" i) :openldk))))
            ;; Get class package from loader - for class specializers
            (class-pkg (if (ldk-loader *context*)
                           (loader-package (ldk-loader *context*))
                           (or (find-package "OPENLDK.SYSTEM")
                               (make-package "OPENLDK.SYSTEM" :use '(:openldk))))))
        ;; Instance methods compile their body as a plain DEFUN and install a
        ;; one-line DEFMETHOD trampoline.  PCL's DEFMETHOD expansion walks the
        ;; whole body (sb-walker) looking for call-next-method — quadratic on
        ;; the huge generated bodies of methods like CLDR resource-bundle
        ;; getContents() (23s for TimeZoneNames_en) — and generated Java
        ;; methods never use call-next-method, so the walk is pure waste.
        (%wrap-method-body
         (static-p method)
         (intern (fn-name *context*) (if (static-p method) class-pkg :openldk))
         (intern (format nil "%jimpl:~A.~A" (slot-value class 'name) (fn-name *context*)) class-pkg)
         (intern (slot-value class 'name) class-pkg)
         args
         (append (when *debug-trace*
                      (list (list 'format 't "~&~V@A <~A> trace: entering ~A.~A(~{~A~^ ~}) ~A~%"
                                  (list 'incf '*call-nesting-level* 1) "*" '*call-nesting-level*
                                  (slot-value class 'name) (fn-name *context*)
                                  (if *debug-trace-args*
                                      (cons 'list args)
                                      ())
                                  (if (not (static-p method)) (intern "this" :openldk) ""))))
                    (let ((i 0)
                          (pc -1))
                      (list (format nil "bridge=~A" (bridge-p method))
                            (%method-let (if (static-p method)
                                                   (append
                                                    (list (list '|condition-cache|) (list '|handler-entry|))
                                                    (remove-duplicates
                                                     (loop for var in (stack-variables *context*)
                                                           unless (or (gethash var (single-assignment-table *context*))
                                                                      (not (stack-variable-is-live-p var blocks-after-dce)))
                                                             collect (list (intern (format nil "s{~{~A~^,~}}"
                                                                                           (sort (copy-list (var-numbers var)) #'<))
                                                                           :openldk)))
                                                     :test #'equal)
                                                    (loop for ph in parameter-hints
                                                          collect (list (intern (format nil "local-~A" i) :openldk)
                                                                        (intern (format nil "arg~A" (incf pc)) :openldk))
                                                          do (if (eq ph t) (incf i) (incf i 2)))
                                                    (loop for pc from (- parameter-count 2) upto max-locals
                                                          collect (list (intern (format nil "local-~A" (1- (incf i))) :openldk))))
                                                   (append
                                                    (list (list '|condition-cache|) (list '|handler-entry|))
                                                    (remove-duplicates
                                                     (loop for var in (stack-variables *context*)
                                                           unless (or (gethash var (single-assignment-table *context*))
                                                                      (not (stack-variable-is-live-p var blocks-after-dce)))
                                                             collect (list (intern (format nil "s{~{~A~^,~}}"
                                                                                           (sort (copy-list (var-numbers var)) #'<))
                                                                           :openldk)))
                                                     :test #'equal)
                                                    (append
                                                     (list (list (intern "local-0" :openldk) (intern "this" :openldk)))
                                                     (loop for ph in parameter-hints
                                                           collect (list (intern (format nil "local-~A" (1+ i)) :openldk)
                                                                         (intern (format nil "arg~A" (1+ (incf pc))) :openldk))
                                                           do (if (eq ph t) (incf i) (incf i 2)))
                                                     (loop for x from parameter-count upto (1+ max-locals)
                                                           collect (list (intern (format nil "local-~A" (incf i)) :openldk))))))
                                    synchronized-lisp-code)))))))))

(defun %straight-line-clinit-p (method)
  "True when METHOD, being compiled in *CONTEXT*, is a <clinit> with no
backward branch: code that runs once, in time bounded by its size, so
SBCL's evaluator runs it in less time than compiling it would take."
  (and (string= (slot-value method 'name) "<clinit>")
       (let ((next (next-insn-list *context*)))
         (loop for pc from 0 below (length next)
               never (some (lambda (target) (<= target pc)) (aref next pc))))))

(defun %install-method-definition (class method method-key definition-code length
                                   &key evaluator)
  "Evaluate DEFINITION-CODE to install METHOD's compiled function, working
around SBCL native-compiler limits on very large method bodies: interpret
huge bytecode outright, and fall back to interpretation when native
compilation fails silently (leaving the self-compiling stub in place).
A straight-line <clinit> is interpreted too: it runs once.
EVALUATOR, when given, is tried before compiling: a function that installs
the method another way (through the FASL cache) and returns true if it did."
  (handler-case
      ;; Very large bytecode (e.g. 1000+ element array initializers)
      ;; can cause SBCL's native compiler to hang or abort.  Use the
      ;; interpreter directly for those methods.
      ;; Through %EVAL every way, which muffles the redefinition warning
      ;; replacing the stub signals: some callers treat a warning as a failure.
      (cond
        ((and (%straight-line-clinit-p method)
              (eq (first definition-code) 'defun)
              (null (third definition-code)))
         ;; Interpret the body, called from a compiled function of the
         ;; method's name: an interpreted function has no frame of its own
         ;; in a backtrace, where getCallerClass and stack traces look.
         (destructuring-bind (name lambda-list &rest body) (rest definition-code)
           (declare (ignore lambda-list))
           (let ((body-function (let ((sb-ext:*evaluator-mode* :interpret))
                                  (%evaluate-java-definition class `(lambda () (block ,name ,@body))))))
             ;; Not a tail call, which would leave no frame either.
             (%evaluate-java-definition class `(defun ,name () (funcall ,body-function) nil)))))
        ((> length 5000)
         (let ((sb-ext:*evaluator-mode* :interpret))
           (%evaluate-java-definition class definition-code)))
        ((and evaluator (funcall evaluator)))
        (t (%evaluate-java-definition class definition-code)))
    (error (c)
      (format *error-output* "~&;; COMPILE-ERROR in ~A: ~A~%" method-key c)
      (force-output *error-output*)
      (error c)))
  ;; SBCL's native compiler can silently fail on very large method
  ;; bodies (e.g. 1000+ element array initializations).  When this
  ;; happens, eval returns without error but the defmethod is never
  ;; installed on the GF — leaving the self-compiling stub in place,
  ;; which causes infinite recursion via invoke-special.  Detect
  ;; this by checking whether the stub was actually replaced, and
  ;; fall back to interpreted evaluation if not.
  (when (not (static-p method))
    (let* ((gf-sym (intern (fn-name *context*) :openldk))
           (gf (and (fboundp gf-sym) (symbol-function gf-sym)))
           (class-sym (intern (slot-value class 'name)
                              (%class-specializer-package class)))
           (class-obj (find-class class-sym nil)))
      (when (and gf class-obj (typep gf 'generic-function))
        (let* ((methods (sb-mop:generic-function-methods gf))
               (our-method (find class-obj methods
                                 :key (lambda (m)
                                        (first (sb-mop:method-specializers m)))
                                 :test #'eq)))
          ;; Check if the method is still the lazy stub -- i.e. it was
          ;; never replaced -- or the trampoline's impl defun (see
          ;; %wrap-method-body) never got installed.
          (when (or (null our-method)
                    (gethash our-method *method-stubs*)
                    (let ((impl-sym (find-symbol (format nil "%jimpl:~A.~A"
                                                         (slot-value class 'name)
                                                         (fn-name *context*))
                                                 (%class-specializer-package class))))
                      (and impl-sym (not (fboundp impl-sym)))))
            (format *error-output*
                    "~&;; COMPILE-FALLBACK: native compilation failed silently for ~A, retrying interpreted~%"
                    method-key)
            (when evaluator
              (ignore-errors (delete-file (%fasl-cache-path class method))))
            (force-output *error-output*)
            (handler-case
                (let ((sb-ext:*evaluator-mode* :interpret))
                  (%evaluate-java-definition class definition-code)) ; lint:suppress eval-usage
              (error (c)
                (format *error-output*
                        "~&;; COMPILE-FALLBACK-ERROR in ~A: ~A~%"
                        method-key c)
                (force-output *error-output*)
                (error c)))))))))

(defun %install-jit-error-stub (class method method-key &optional java-condition)
  "Install a stable failure instead of leaving a self-recursing JIT stub.
Preserve an existing Java linkage condition without entering Java to construct
another exception; internal compiler failures continue to use Lisp errors."
  ;; Instance methods dispatch on a generic function in :openldk, where the
  ;; lazy compile stub and %build-method-definition both put them.  Interning
  ;; in the loader's package instead (OPENLDK.APP for the classpath) put the
  ;; error stub on an unrelated generic function, so the lazy stub survived and
  ;; every call looped: %compile-method returns at once for a :done method.
  (when (%user-ldk-loader-p (ldk-loader class))
    (let* ((count (count-parameters (descriptor method)))
           (arguments (loop repeat (+ count (if (static-p method) 0 1)) collect (gensym "ARG")))
           (symbol (intern (%method-fn-name class method)
                           (if (static-p method) (ldk-class-package class) :openldk)))
           (implementation (if (static-p method) symbol
                               (intern (format nil "%jerror:~A" method-key) (ldk-class-package class))))
           (function (lambda (&rest values)
                       (declare (ignore values))
                       (if java-condition (error java-condition)
                           (internal-error "JIT compilation failed for ~A" method-key)))))
      (%install-owned-java-function class implementation function)
      (unless (static-p method)
        (%eval `(defmethod ,symbol ((,(first arguments) ,(intern (name class) (ldk-class-package class))) ,@(rest arguments))
                  (,implementation ,@arguments)))))
    (return-from %install-jit-error-stub nil))
  (let ((fn-symbol (intern (%method-fn-name class method)
                           (if (static-p method)
                               (%class-loader-package class)
                               :openldk))))
    (if (static-p method)
        (when (fboundp fn-symbol)
          (setf (symbol-function fn-symbol)
                (lambda (&rest args)
                  (declare (ignore args))
                  (if java-condition (error java-condition)
                      (internal-error "JIT compilation failed for ~A" method-key)))))
        ;; Instance method: install a defmethod that throws an error.
        ;; Use CL:EVAL (not %eval) to avoid re-entering the JIT.
        (let* ((class-sym (intern (slot-value class 'name)
                                  (%class-specializer-package class)))
               (this-sym (intern "this" :openldk))
               (n-params (count-parameters (slot-value method 'descriptor)))
               (param-names (loop for i from 1 upto n-params
                                  collect (gensym (format nil "P~A-" i)))))
          (when (find-class class-sym nil)
            (eval `(defmethod ,fn-symbol ((,this-sym ,class-sym) ,@param-names)
                     (declare (ignore ,this-sym ,@param-names))
                     (if ,java-condition (error ,java-condition)
                         (internal-error "JIT compilation failed for ~A" ,method-key)))))))))

(defun %compile-method (class-or-name method-index)
  "JIT-compile method METHOD-INDEX of CLASS-OR-NAME (a <class>, or in AOT
output a class name): transpile its bytecode to IR, optimize, generate Lisp
code, and install the resulting function (or write it out in AOT mode).
Thread-safe; concurrent callers wait for or skip in-progress compilations.
Method stubs pass the <class> itself, because two class loaders can define
classes with the same name."
  (let* ((class (if (stringp class-or-name)
                    (%get-ldk-class-by-bin-name class-or-name)
                    class-or-name))
         (class-name (slot-value class 'name))
         (method (aref (slot-value class 'methods) (1- method-index)))
         (method-key (format nil "~A.~A~A" class-name (slot-value method 'name) (slot-value method 'descriptor)))
         ;; Compile claims for a user loader's classes also carry the
         ;; loader, so a same-named class in another loader has its own.
         (claim-key (let ((loader (and (slot-boundp class 'ldk-loader)
                                       (slot-value class 'ldk-loader))))
                      (if (%user-ldk-loader-p loader)
                          (cons (id loader) method-key)
                          method-key))))
    (%verify-class-for-java class)
    ;; Skip bytecode compilation for methods with native overrides.
    ;; Replace the stub with the native function so the stub doesn't loop.
    (when (%install-native-override class method method-key)
      (return-from %compile-method nil))
    (unless (%claim-method-compilation claim-key)
      (return-from %compile-method nil))
    (let ((compilation-completed nil))
      (unwind-protect
           (handler-case
               (handler-bind
                   ((error (lambda (c)
                             (declare (ignore c))
                             ;; Print the backtrace where the error is
                             ;; signalled, before HANDLER-CASE unwinds it,
                             ;; to the process's stderr: compilation can
                             ;; rebind *ERROR-OUTPUT* to muffle the compiler.
                             (when *debug-codegen*
                               (sb-debug:print-backtrace :stream sb-sys:*stderr* :count 60)))))
                 (when (gethash "Code" (slot-value method 'attributes)) ; otherwise it is abstract
                   (let* ((compile-start-time (get-internal-real-time))
                          (code-attribute (gethash "Code" (slot-value method 'attributes)))
                          (parameter-hints (gen-parameter-hints (descriptor method)))
                          (code (slot-value code-attribute 'code))
                          (max-locals (slot-value code-attribute 'max-locals))
                          (length (length code))
                          (*context* (make-instance '<context>
                                                    :class class
                                                    :ldk-loader (slot-value class 'ldk-loader)
                                                    :classes *ldk-classes-by-bin-name*
                                                    :exception-table (slot-value code-attribute 'exceptions)
                                                    :bytecode code
                                                    :insn-size (make-array (length code) :element-type 'fixnum :initial-element -1)
                                                    :next-insn-list (make-array (length code) :initial-element nil)
                                                    :stack-state-table (make-hash-table)
                                                    :pc 0
                                                    :is-clinit-p (string= "<clinit>" (slot-value method 'name))
                                                    :compact-p (or (string= "<clinit>" (slot-value method 'name))
                                                                   (> length *compact-method-length*)))))
                     (setf (svcount *context*) 0)
                     (when *debug-bytecode*
                       (format t "~&; COMPILING ~A~%" method-key))
                     (setf (fn-name *context*) (%method-fn-name class method))
                     (let ((cacheable (%fasl-cache-eligible-p class method length)))
                       (unless (and cacheable
                                    (not *fasl-cache-verify*)
                                    (%fasl-cache-load class method))
                         (let* ((dependencies (and cacheable (make-hash-table :test #'equal)))
                                (call-site-gfs (and cacheable (make-hash-table :test #'eq)))
                                (definition-code
                                  (let ((*compile-dependencies* dependencies)
                                        (*compile-call-site-gfs* call-site-gfs))
                                    (%build-method-definition
                                     class method
                                     (%optimize-method-blocks
                                      (setf (ir-code *context*)
                                            (%transpile-with-stack-hints method-key code length)))
                                     parameter-hints max-locals))))
                           (if *aot-dir*
                               (%write-aot-method class-name
                                                  (lispize-method-name (format nil "~A~A" (name method) (descriptor method)))
                                                  definition-code)
                               (%install-method-definition
                                class method method-key definition-code length
                                :evaluator (and cacheable
                                                (lambda ()
                                                  (%fasl-cache-install class method definition-code
                                                                       dependencies call-site-gfs)))))))
                       (when (or *debug-compile* *debug-codegen*)
                         (format t "; COMPILING ~A.~A (~Dms)~%"
                                 class-name
                                 (lispize-method-name (format nil "~A~A" (name method) (descriptor method)))
                                 (round (* 1000 (/ (- (get-internal-real-time) compile-start-time)
                                                   internal-time-units-per-second))))
                         (force-output)))))
                 (setf compilation-completed t))
             (|condition-java/lang/LinkageError| (condition)
               ;; Eager linking during compilation is still a Java linkage
               ;; failure, not a Lisp compiler error. Preserve it on every call.
               (%install-jit-error-stub class method method-key condition)
               (setf compilation-completed t))
             (error (c)
               ;; Compilation failed (IR translation, codegen, or eval error).
               (format *error-output* "~&;; JIT-ERROR in ~A: ~A~%" method-key c)
               (when (typep c '|condition-java/lang/Throwable|)
                 (let ((throwable (slot-value c '|objref|)))
                   (format *error-output* ";; ~A: ~A~%"
                           (class-name (class-of throwable))
                           (lstring (slot-value throwable '|detailMessage|)))))
               (force-output *error-output*)
               (%install-jit-error-stub class method method-key)
               (setf compilation-completed t)))
        ;; Cleanup: mark compilation as done and notify waiting threads.
        ;; If compilation was interrupted (e.g. by timeout or stack overflow),
        ;; clear the status so a future call can retry.
        (bt:with-lock-held (*method-compilation-lock*)
          (if compilation-completed
              (setf (gethash claim-key *methods-being-compiled*) :done)
              (remhash claim-key *methods-being-compiled*))
          ;; Broadcast: the condition variable is shared by every method,
          ;; so waiters on other methods, and several waiters on this one,
          ;; must all get to re-check their method's status.
          (sb-thread:condition-broadcast *method-compilation-cv*))))))


(defun %ensure-class-initialized (class clinit-fn)
  "Run CLINIT-FN (a class's <clinit>) at most once, with JVM class-init
locking: concurrent initializers block until it completes, while the
initializing thread may re-enter (its own <clinit> touching the class).
INITIALIZED-P is set only AFTER <clinit> finishes, so another thread never
sees the class as initialized while its statics are still unset -- the race
that made concurrent Math.random()/holder-class first-use throw NPE."
  (unless (initialized-p class)
    (bordeaux-threads:with-recursive-lock-held ((%get-class-lock (slot-value class 'name)))
      (cond
        ((init-error class)
         (%throw-class-init-failed class))
        ((or (initialized-p class) (initializing-p class)))
        (t
         (setf (initializing-p class) t)
         (unwind-protect
              (handler-case (funcall clinit-fn)
                (|condition-java/lang/Throwable| (c)
                  (if (slot-boundp c '|objref|)
                      (%fail-class-init class (slot-value c '|objref|))
                      (error c))))
           ;; A Lisp-level failure (an internal error, a warmup's timeout)
           ;; leaves the class usable, as it always has.
           (setf (initializing-p class) nil)
           (unless (init-error class)
             (setf (initialized-p class) t))))))))

(defun %class-init-error (throwable)
  "What a <clinit> that threw THROWABLE throws (JLS 12.4.2): THROWABLE itself
if it is an Error, else an ExceptionInInitializerError wrapping it."
  (if (typep throwable '|java/lang/Error|)
      throwable
      (progn
        (classload "java/lang/ExceptionInInitializerError")
        (let ((eiie (%make-java-instance "java/lang/ExceptionInInitializerError")))
          (|<init>(Ljava/lang/Throwable;)V| eiie throwable)
          eiie))))

(defun %fail-class-init (class throwable)
  "Mark CLASS erroneous, its <clinit> having thrown THROWABLE, and throw
what that <clinit> throws.  Later uses throw NoClassDefFoundError."
  (setf (init-error class) throwable
        (initialized-p class) nil)
  (error (%lisp-condition (%class-init-error throwable))))

(defun %throw-class-init-failed (class)
  "Throw the NoClassDefFoundError a JVM throws on using CLASS, whose
<clinit> failed.  As HotSpot's, its cause is an ExceptionInInitializerError
naming what the <clinit> threw."
  (classload "java/lang/NoClassDefFoundError")
  (classload "java/lang/ExceptionInInitializerError")
  (let* ((thrown (init-error class))
         (message (and (slot-exists-p thrown '|detailMessage|)
                       (slot-value thrown '|detailMessage|)))
         (cause (%make-java-instance "java/lang/ExceptionInInitializerError"))
         (ncdfe (%make-java-instance "java/lang/NoClassDefFoundError")))
    (|<init>(Ljava/lang/String;)V| cause
     (jstring (format nil "Exception ~A~@[: ~A~]"
                      (substitute #\. #\/ (symbol-name (class-name (class-of thrown))))
                      (and message (lstring message)))))
    (|<init>(Ljava/lang/String;)V| ncdfe
     (jstring (format nil "Could not initialize class ~A" (substitute #\. #\/ (name class)))))
    (setf (slot-value ncdfe '|cause|) cause)
    (error (%lisp-condition ncdfe))))

(defun %clinit (class)
  (%verify-class-for-java class)
  (let ()
    (assert
     (or class (internal-error "Can't find ~A" class)))
    (labels ((clinit (class)
               (let ((super-class (and (super class)
                                       (%resolve-class (super class) (ldk-loader class)))))
                 (when (and super-class (not (initialized-p super-class)))
                   (clinit super-class)))
               (let ((<clinit>-method (find-if
                                       (lambda (method)
                                         (and (string= (slot-value method 'name) "<clinit>")
                                              (string= (slot-value method 'descriptor) "()V")))
                                       (slot-value class 'methods))))
                 ;; Per JVM spec: if class init is already in progress
                 ;; by the current thread, return immediately.
                 ;; Use per-class lock to prevent concurrent initialization.
                 (when <clinit>-method
                   (bordeaux-threads:with-recursive-lock-held ((%get-class-lock (slot-value class 'name)))
                     ;; Skip if this class is already initialized OR currently
                     ;; being initialized on this thread.  The JIT path
                     ;; (%ensure-class-initialized) marks INITIALIZING-P during a
                     ;; <clinit> and only sets INITIALIZED-P afterward; without the
                     ;; INITIALIZING-P check here, a reentrant %clinit for a class
                     ;; whose <clinit> is already running via that path would RE-RUN
                     ;; the <clinit>, re-executing its static initializer.  For
                     ;; clojure.lang.Namespace that reassigns the static `namespaces`
                     ;; registry to a fresh empty map, dropping already-registered
                     ;; namespaces (e.g. clojure.core.protocols during clojure.core
                     ;; bootstrap) -> "Unable to resolve" at core.clj:6866. (ldk-ia9)
                     (when (init-error class)
                       (%throw-class-init-failed class))
                     (when (not (or (initialized-p class) (initializing-p class)))
                       (when (search "TwoWayStream" (slot-value class 'name))
                         (format *error-output* "~&;; CLINIT: initializing ~A~%" (slot-value class 'name))
                         (force-output *error-output*))
                       (setf (initialized-p class) t)
                       (handler-case
                       ;; Get package from loader if available, else use OPENLDK.SYSTEM
                       ;; (classes loaded during warm-up may not have ldk-loader set)
                       (let ((pkg (if (and (slot-boundp class 'ldk-loader)
                                           (slot-value class 'ldk-loader))
                                      (loader-package (slot-value class 'ldk-loader))
                                      (or (find-package "OPENLDK.SYSTEM")
                                          (make-package "OPENLDK.SYSTEM" :use '(:openldk))))))
                         (%eval (list (intern (format nil "~A.<clinit>()V" (slot-value class 'name)) pkg)))
                         ;; Run post-clinit hook if registered (for VM-injected fields)
                         (let ((hook (gethash (slot-value class 'name) *post-clinit-hooks*)))
                           (when hook (funcall hook class pkg))))
                     (error (e)
                       (let ((throwable (when (and (typep e '|condition-java/lang/Throwable|)
                                                   (slot-boundp e '|objref|))
                                          (slot-value e '|objref|))))
                         (format *error-output* "~&;; <clinit> ERROR in ~A: ~A~%"
                                 (slot-value class 'name) e)
                         (force-output *error-output*)
                         (when (or *debug-codegen* *debug-exceptions*)
                           (if throwable
                               (%print-java-stack-trace throwable :stream *error-output*)
                               (trivial-backtrace:print-backtrace e :output *error-output*)))
                         ;; A Java exception fails the class for good.
                         (when (typep throwable '|java/lang/Throwable|)
                           (%fail-class-init class throwable))
                         ;; Wrap exception in ExceptionInInitializerError if classes are loaded
                         ;; Use ignore-errors to handle case where parent class isn't loaded yet
                         (let ((eiie (ignore-errors
                                       (when (and (find-class '|java/lang/ExceptionInInitializerError| nil)
                                                  (find-class '|java/lang/Error| nil))
                                         (let* ((instance (%make-java-instance "java/lang/ExceptionInInitializerError"))
                                                (cause (and (typep throwable '|java/lang/Throwable|) throwable)))
                                           (if cause
                                               (|<init>(Ljava/lang/Throwable;)V| instance cause)
                                               (|<init>()V| instance))
                                           ;; Ensure cause/exception slots are populated for stack traces
                                           (when cause
                                             (when (slot-exists-p instance '|cause|)
                                               (setf (slot-value instance '|cause|) cause))
                                             (when (slot-exists-p instance '|exception|)
                                               (setf (slot-value instance '|exception|) cause)))
                                           instance)))))
                           (if eiie
                               (error (%lisp-condition eiie))
                               ;; During early bootstrap, just re-signal the original error
                               (error e))))))))))))
      (clinit class))))

(defun open-java-classfile-on-classpath (class)
  (let* ((class (substitute (uiop:directory-separator-for-host) #\. class)))
    (loop for cpe in *classpath*
          for classfile-stream = (open-java-classfile cpe class)
          when classfile-stream
            return classfile-stream)))

(defun initform-from-descriptor (descriptor)
  (cond
    ((string= descriptor "I")
     0)
    ((string= descriptor "J")
     0)
    ((string= descriptor "F")
     0.0)
    ((string= descriptor "D")
     0.0d0)
    ((string= descriptor "S")
     0)
    ((string= descriptor "B")
     0)
    ((string= descriptor "C")
     0)
    ((string= descriptor "Z")
     0)
    (t nil)))

(defun mangle-field-name (name)
  "Mangle Java field names that conflict with Common Lisp constants/special symbols."
  (let ((upcase-name (string-upcase name)))
    ;; Check if this would conflict with CL constants or special symbols
    (if (member upcase-name
                '("T" "NIL" "CLASS" "METHOD" "PACKAGE" "TYPE"
                  "IF" "QUOTE" "LAMBDA" "BLOCK" "RETURN-FROM"
                  "CATCH" "THROW" "UNWIND-PROTECT" "TAGBODY" "GO"
                  "LET" "LET*" "SETQ" "PROGN" "PROG1" "PROG2"
                  "FUNCTION" "EVAL-WHEN" "LOAD-TIME-VALUE" "LOCALLY"
                  "MACROLET" "MULTIPLE-VALUE-CALL" "MULTIPLE-VALUE-PROG1"
                  "PROGV" "SETF" "THE" "SYMBOL-MACROLET" "DECLARE")
                :test #'string=)
        ;; Mangle by appending "$" suffix
        (concatenate 'string name "$")
        ;; No conflict, return as-is
        name)))

(defun %remove-redundant-interfaces (ifaces)
  "Remove interfaces already inherited through other interfaces in the list.
   If Indexed extends Counted, and both are in the list, remove Counted."
  (remove-if (lambda (iface)
               (let ((fc-iface (find-class iface nil)))
                 (and fc-iface
                      (some (lambda (other)
                              (and (not (eq iface other))
                                   (let ((fc-other (find-class other nil)))
                                     (and fc-other
                                          (subtypep fc-other fc-iface)
                                          (not (subtypep fc-iface fc-other))))))
                            ifaces))))
             ifaces))

(defvar *supertype-counts* (make-hash-table :test #'equal :synchronized t)
  "Binary class name -> how many distinct supertypes it has.")

(defun %supertype-count (class-name)
  "How many distinct supertypes (superclasses and superinterfaces, all the
way up) the class CLASS-NAME has, from its class file: more for any
subtype, and fixed once the class is loaded."
  (or (gethash class-name *supertype-counts*)
      (setf (gethash class-name *supertype-counts*)
            (let ((seen (make-hash-table :test #'equal)))
              (labels ((walk (name)
                         (let ((ldk-class (gethash name *ldk-classes-by-bin-name*)))
                           (when ldk-class
                             (dolist (super (append (and (slot-value ldk-class 'super)
                                                         (list (slot-value ldk-class 'super)))
                                                    (coerce (interfaces ldk-class) 'list)))
                               (unless (gethash super seen)
                                 (setf (gethash super seen) t)
                                 (walk super)))))))
                (walk class-name))
              (hash-table-count seen)))))

(defun %topo-sort-interfaces (ifaces)
  "Order interface symbols so that CLOS can always compute class precedence
lists: removing redundant interfaces, then sorting by one global order that
agrees with subtyping -- most supertypes (most specific) first, then by
name.  Java gives the order of a class's interfaces no
meaning, but CLOS requires the precedence list to agree with every class's
order of direct superclasses; ordering each class's interfaces as Java
lists them made some combinations contradict (IntelliJ's PSI and the
Kotlin compiler's Java model, which list the same interfaces in opposite
orders), while one order shared by all classes never can."
  (when (null ifaces)
    (return-from %topo-sort-interfaces nil))
  (flet ((depth (sym) (%supertype-count (symbol-name sym))))
    (stable-sort (copy-list (%remove-redundant-interfaces ifaces))
                 (lambda (a b)
                   (let ((da (depth a)) (db (depth b)))
                     (or (> da db)
                         (and (= da db) (string< (symbol-name a) (symbol-name b)))))))))

(defun %field-shadows-super-p (class field ldk-loader)
  "True when instance FIELD of CLASS shadows an instance field of the same
name declared somewhere in CLASS's superclass chain (distinct fields in
the JVM -- javac's synthetic this$0 outer references do this routinely)."
  (and (zerop (logand 8 (slot-value field 'access-flags)))   ; instance field
       (let ((field-name (slot-value field 'name)))
         (loop for super-class = (and (super class) (%resolve-class (super class) ldk-loader))
                 then (and (super super-class)
                           (%resolve-class (super super-class) (ldk-loader super-class)))
               while super-class
               when (find-if (lambda (sf)
                               (and (string= (slot-value sf 'name) field-name)
                                    (zerop (logand 8 (slot-value sf 'access-flags)))))
                             (fields super-class))
                 return t))))

(defun %field-slot-symbol (class field ldk-loader)
  "CLOS slot symbol for FIELD of CLASS: the plain mangled name, or a
class-qualified symbol (recorded in *FIELD-SHADOW-SLOTS*) when the field
shadows a superclass field and needs its own storage."
  (let ((mangled (mangle-field-name (slot-value field 'name))))
    (if (%field-shadows-super-p class field ldk-loader)
        (let ((sym (intern (format nil "~A$~A" mangled (slot-value class 'name)) :openldk)))
          (setf (gethash (slot-value field 'name) (%owner-cache-table *field-shadow-slots* class))
                sym)
          sym)
        (intern mangled :openldk))))

(defun emit-<class> (class ldk-loader)
  "Emit the CLOS class definition for a Java class, and the defparameter
   of its static instance.  %INSTALL-METHOD-STUBS installs its methods.
   LDK-LOADER is the class loader that will own this class.
   Class symbols are interned in the loader's package for isolation.
   Generic function names stay in :openldk for dispatch to work across loaders."
  (let* ((pkg (loader-package ldk-loader))
         (defclass-code
           (with-slots (name super interfaces fields) class
             (list
              'progn
              (list
               'defclass (intern name pkg)  ; This class in loader's package
               (let ((supers
                      (if (or super interfaces)
                          (append (when super
                                    (list (class-symbol-for-reference super ldk-loader)))
                                  (let ((ifaces (remove-if-not
                                                 (lambda (sym) (find-class sym nil))
                                                 (mapcar (lambda (i)
                                                           (class-symbol-for-reference i ldk-loader))
                                                         (coerce interfaces 'list)))))
                                    (%topo-sort-interfaces ifaces)))
                          (list))))
                ;; JDK 9+: jdk/internal/misc/Unsafe must inherit from sun/misc/Unsafe
                ;; so that existing native method stubs dispatch correctly.
                ;; Replace java/lang/Object with sun/misc/Unsafe (which itself extends Object).
                (when (and (string= name "jdk/internal/misc/Unsafe")
                           (find-class '|sun/misc/Unsafe| nil)
                           (not (member '|sun/misc/Unsafe| supers)))
                  (setf supers (substitute '|sun/misc/Unsafe| '|java/lang/Object| supers)))
                supers)
               ;; Field names stay in :openldk (slots are inherited across packages)
               (append (when (or (string= name "java/lang/Object") (%user-ldk-loader-p ldk-loader))
                         (list (list *java-loader-owner-slot* :initform nil)))
                       (map 'list
                    (lambda (f)
                      (list (%field-slot-symbol class f ldk-loader)
                            :initform (let ((cf (gethash "ConstantValue" (slot-value f 'attributes))))
                                        (if cf
                                            (value (emit (aref (constant-pool class) cf) (constant-pool class)))
                                            (initform-from-descriptor (slot-value f 'descriptor))))
                            :allocation
                            (if (or (eq 0 (logand 8 (slot-value f 'access-flags)))
                                    (%user-ldk-loader-p ldk-loader)) :instance :class)))
                    fields)))
              (if (%user-ldk-loader-p ldk-loader)
                  `(progn
                     (define-symbol-macro ,(intern (format nil "+static-~A+" name) pkg)
                       (%class-static-instance-by-symbol ',(intern name pkg)))
                     (%register-owned-static-instance ',(intern name pkg) (make-instance ',(intern name pkg))))
                  (list
                   'defparameter (intern (format nil "+static-~A+" (intern name pkg)) pkg)
                   (list
                    'make-instance (list 'quote (intern name pkg)))))))))
    defclass-code))

(defvar *method-stubs* (make-hash-table :test 'eq :weakness :key :synchronized t)
  "Lazy-compilation stub methods installed by %INSTALL-METHOD-STUBS, so
%INSTALL-METHOD-DEFINITION can tell whether compiling replaced one.")

(defun %reentrant-method-compilation-p (class-ref method-index)
  "True if this thread is already compiling the method its stub is entering."
  (let* ((class (if (stringp class-ref) (%get-ldk-class-by-bin-name class-ref) class-ref))
         (method (aref (methods class) (1- method-index)))
         (name (format nil "~A.~A~A" (name class) (name method) (descriptor method)))
         (loader (ldk-loader class))
         (key (if (%user-ldk-loader-p loader) (cons (id loader) name) name)))
    (bt:with-lock-held (*method-compilation-lock*)
      (eq (gethash key *methods-being-compiled*) (bt:current-thread)))))

(defun %static-method-stub (class-ref method-index fn-symbol)
  "A static method's lazy stub: compile method METHOD-INDEX of CLASS-REF,
which replaces FN-SYMBOL's definition, then call the new definition."
  (lambda (&rest args)
    (let ((class-ref (%resolve-class-reference class-ref)))
    (if (%reentrant-method-compilation-p class-ref method-index)
        (%execute-verified-bytecode class-ref method-index nil args)
        (progn
          (%compile-method class-ref method-index)
          (apply fn-symbol args))))))

(defun %instance-method-stub (gf class-ref method-index method-symbol class-symbol param-count)
  "A standard method on GF specialized on CLASS-SYMBOL whose function compiles
method METHOD-INDEX of CLASS-REF, which replaces this method, then invokes
the new one."
  (let ((method (make-instance (sb-mop:generic-function-method-class gf)
                               :qualifiers nil
                               :lambda-list (cons (intern "this" :openldk)
                                                  (loop for i from 1 upto param-count
                                                        collect (intern (format nil "arg~A" i) :openldk)))
                               :specializers (cons (find-class class-symbol)
                                                   (make-list param-count :initial-element (find-class t)))
                               :function (lambda (args next-methods)
                                           (declare (ignore next-methods))
                                           (let ((class-ref (%resolve-class-reference class-ref)))
                                           (if (%reentrant-method-compilation-p class-ref method-index)
                                               (%execute-verified-bytecode class-ref method-index (first args) (rest args))
                                               (progn
                                                 (%compile-method class-ref method-index)
                                                 (invoke-special method-symbol class-symbol args))))))))
    (setf (gethash method *method-stubs*) t)
    method))

(defun %install-method-stubs (class ldk-loader)
  "Install a lazy-compilation stub for each of CLASS's methods that has
bytecode to compile: a function for a static method, a method on its
generic function for an instance method.  The stubs are closures, made
without running the compiler, since most methods are never called."
  (%install-jni-method-stubs class ldk-loader)
  (unless (equal (name class) "java/util/zip/ZipFile")
    (let ((pkg (loader-package ldk-loader))
          (class-ref (%stub-class-reference class))
          (done-method-table (make-hash-table :test #'equal)))
      (loop for m across (methods class)
            for method-index from 1
            for method-name = (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor)))
            unless (or (native-p m)
                       (native-override-p m)
                       (null (gethash "Code" (attributes m)))
                       (and (bridge-p m) (gethash method-name done-method-table)))
              do (setf (gethash method-name done-method-table) t)
                 (if (static-p m)
                     ;; Static methods: function name in loader's package (includes class name)
                     (let ((fn-symbol (intern (format nil "~A.~A" (slot-value class 'name) method-name) pkg)))
                       (setf (fdefinition fn-symbol) (%static-method-stub class-ref method-index fn-symbol)))
                     ;; Instance methods: generic function name in :openldk for dispatch,
                     ;; class specializer in loader's package for isolation.
                     (let* ((method-symbol (intern method-name :openldk))
                            (param-count (count-parameters (slot-value m 'descriptor)))
                            (class-symbol (intern (slot-value class 'name) pkg)))
                       (%ensure-java-gf method-symbol (1+ param-count))
                       (let ((gf (fdefinition method-symbol)))
                         (add-method gf (%instance-method-stub gf class-ref method-index method-symbol
                                                               class-symbol param-count)))))))))


(defun find-class-in-loader-hierarchy (classname ldk-loader)
  "Search for class in this loader and all parent loaders sharing the same package."
  (%lookup-class-with-delegation ldk-loader classname 'ldk-classes-by-bin-name))

(defun %publish-class-globally-p (classname loader)
  "Keep boot/app definitions in compatibility tables when a child shadows them."
  (or (not (%user-ldk-loader-p loader))
      (let ((existing (gethash classname *ldk-classes-by-bin-name*)))
        (or (null existing) (%defined-by-user-loader-p existing 'ldk-classes-by-bin-name)))))

(defun %same-runtime-package-p (left right)
  "Package identity includes the defining class loader."
  (and (eq (ldk-loader left) (ldk-loader right))
       (string= (subseq (name left) 0 (or (position #\/ (name left) :from-end t) 0))
                (subseq (name right) 0 (or (position #\/ (name right) :from-end t) 0)))))

(defun %check-class-inheritance (class super interfaces)
  "Enforce superclass/interface kinds and the JVM's final inheritance rules."
  (when super
    (when (or (logbitp 4 (access-flags super)) (interface-p super))
      (%throw '|java/lang/IncompatibleClassChangeError|
              (format nil "~A has invalid superclass ~A" (name class) (name super))))
    (loop for parent = super then (and (super parent) (%resolve-class (super parent) (ldk-loader parent)))
          while parent
          do (loop for method across (methods class)
                   unless (or (static-p method) (string= (name method) "<init>")
                              (logbitp 1 (access-flags method)))
                     do (let ((inherited (find-if
                                          (lambda (candidate)
                                            (and (string= (name method) (name candidate))
                                                 (string= (descriptor method) (descriptor candidate))))
                                          (methods parent))))
                          (when (and inherited (logbitp 4 (access-flags inherited))
                                     (not (static-p inherited))
                                     (not (logbitp 1 (access-flags inherited)))
                                     (or (not (zerop (logand (access-flags inherited) #x5)))
                                         (%same-runtime-package-p class parent)))
                            (%throw '|java/lang/IncompatibleClassChangeError|
                                    (format nil "~A overrides final method ~A.~A"
                                            (name class) (name parent) (name inherited))))))))
  (dolist (parent interfaces)
    (when (and parent (not (interface-p parent)))
      (%throw '|java/lang/IncompatibleClassChangeError|
              (format nil "~A has non-interface parent ~A" (name class) (name parent))))))

(defun %check-sealed-supertypes (class parents)
  "Enforce permitted direct subtypes and runtime package/module membership."
  (dolist (parent parents)
    (when parent
      (multiple-value-bind (permitted sealed-p) (gethash "PermittedSubclasses" (attributes parent))
        (when sealed-p
          (let* ((loader (ldk-loader class))
                 (parent-loader (ldk-loader parent))
                 (module (%defining-class-module (name parent) parent-loader (java-loader parent-loader)))
                 (named-p (and module (slot-value module '|name|)))
                 (image-module (when (zerop (id parent-loader))
                                 (%runtime-image-class-module-name (name parent)))))
            (unless (and (member (name class) permitted :test #'string=)
                         ;; Bootstrap metadata may predate INITIALIZE's loader
                         ;; object; id zero always denotes the one boot loader.
                         (or (eq loader parent-loader)
                             (and (zerop (id loader)) (zerop (id parent-loader))))
                         (cond
                           (named-p (eq module (%defining-class-module (name class) loader (java-loader loader))))
                           (image-module (equal image-module (%runtime-image-class-module-name (name class))))
                           (t (equal (subseq (name class) 0 (or (position #\/ (name class) :from-end t) 0))
                                     (subseq (name parent) 0 (or (position #\/ (name parent) :from-end t) 0))))))
              (%throw '|java/lang/IncompatibleClassChangeError|
                      (format nil "~A cannot inherit from sealed type ~A" (name class) (name parent))))))))))

(defun %classload-from-stream (classname classfile-stream class-loader ldk-loader)
  "Load a class from a stream. CLASS-LOADER is the java.lang.ClassLoader object.
   LDK-LOADER is the <ldk-class-loader> to use for this class."
  ;; Check if CLOS class already defined in the loader's package
  ;; This handles user loaders that share the same package
  ;; Search up the loader hierarchy to find the class from parent loaders
  (when ldk-loader
    (let* ((pkg (loader-package ldk-loader))
           (classname-symbol (find-symbol classname pkg)))
      (when (and classname-symbol (find-class classname-symbol nil))
        ;; Class already defined - search loader hierarchy to find it
        (let ((existing-ldk-class (find-class-in-loader-hierarchy classname ldk-loader)))
          (when existing-ldk-class
            (return-from %classload-from-stream existing-ldk-class))))))
  (let ((definition nil) (completed nil))
    (unwind-protect
       (let* ((publish-globally (%publish-class-globally-p classname ldk-loader))
              (pkg (loader-package ldk-loader))
              (classname-symbol (intern classname pkg))
              (fq-classname (cl-ppcre:regex-replace-all "\\.anonymous-class"
                                                        (substitute #\. #\/ classname)
                                                        "/anonymous-class"))
              (class
                (let ((c (handler-case (read-classfile classfile-stream)
                           (bytecode-verification-error (condition)
                             (%throw-verification-error condition)))))
                  (setf (name c) classname)
                  (setf (slot-value c 'ldk-loader) ldk-loader)
                  (setf definition c)
                  (%record-initiating-class c ldk-loader)
                  ;; Store in loader's hash tables
                  (setf (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name)) c)
                  (setf (gethash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name)) c)
                  ;; Also store in global tables for backward compatibility
                  ;; This allows lookups without a specific loader to find dynamically loaded classes
                  (when publish-globally
                    (setf (gethash classname *ldk-classes-by-bin-name*) c)
                    (setf (gethash fq-classname *ldk-classes-by-fq-name*) c))
                  c))
              ;; Resolve through the defining loader, which may be the only
              ;; one that can find them.
              (super (let ((super (slot-value class 'super)))
                       (when super (%resolve-class super ldk-loader))))
              (interfaces (let ((interfaces (slot-value class 'interfaces)))
                            (when interfaces
                              (mapcar (lambda (i) (%resolve-class i ldk-loader))
                                      (coerce interfaces 'list))))))
         ;; If superclass couldn't be loaded, bail out (like JVM NoClassDefFoundError).
         ;; Clean up the partially-registered class entries and return nil.
         (when (and (slot-value class 'super) (null super))
           (remhash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))
           (remhash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name))
           (when publish-globally
             (remhash classname *ldk-classes-by-bin-name*)
             (remhash fq-classname *ldk-classes-by-fq-name*))
           (return-from %classload-from-stream nil))
         (handler-case
             (progn
               (%check-class-inheritance class super interfaces)
               (%prepare-loader-constraints class)
               (%check-sealed-supertypes class (cons super interfaces)))
           (error (condition)
             ;; A rejected definition must not poison later attempts in this loader.
             (remhash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))
             (remhash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name))
             (when publish-globally
               (remhash classname *ldk-classes-by-bin-name*)
               (remhash fq-classname *ldk-classes-by-fq-name*))
             (error condition)))
         (let ((klass (or (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name))
                          (let ((klass (%make-java-instance "java/lang/Class"))
                                (cname (jstring fq-classname)))
                            (with-slots (|name| |classLoader|) klass
                              (setf |name| cname)
                              (setf |classLoader| (or class-loader
                                                     (and (eq ldk-loader *app-ldk-class-loader*)
                                                          (java-loader ldk-loader)))))
                            klass))))
           (setf (java-class class) klass)
           (when (%user-ldk-loader-p ldk-loader)
             (setf (slot-value klass *java-loader-owner-slot*) ldk-loader))
           ;; JDK 25: Class.getModifiers(), isInterface() and isEnum() read the
           ;; `modifiers` field directly (these are no longer native methods), so
           ;; populate it from the class file's access flags.
           (when (slot-exists-p klass '|modifiers|)
             ;; Class.getModifiers() reports source-level modifiers, not raw class
             ;; access flags: mask out ACC_SUPER (0x20), set on every modern class,
             ;; which would otherwise render as "synchronized".
             ;; A nested class's come from its InnerClasses entry, which
             ;; has static, private and protected, as on a JVM.
             (setf (slot-value klass '|modifiers|)
                   (logandc2 (let ((entry (%inner-class-entry class)))
                               (if entry
                                   (slot-value entry 'inner-class-access-flags)
                                   (access-flags class)))
                             #x20)))
           ;; The application's classes report the system class loader, as on
           ;; a JVM, where code asks it for their resources (Groovy's
           ;; META-INF/dgminfo).
           (setf (slot-value klass '|classLoader|)
                 (or class-loader
                     (and (eq ldk-loader *app-ldk-class-loader*) *boot-class-loader*)))
           (when (and *unnamed-module* (slot-exists-p klass '|module|))
             (setf (slot-value klass '|module|)
                   (%defining-class-module classname ldk-loader (slot-value klass '|classLoader|))))
           ;; Store in loader's java-class hash tables
           (setf (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name)) klass)
           (setf (gethash fq-classname (slot-value ldk-loader 'java-classes-by-fq-name)) klass)
           ;; Also store in global tables for backward compatibility
           (when publish-globally
             (setf (gethash classname *java-classes-by-bin-name*) klass)
             (setf (gethash fq-classname *java-classes-by-fq-name*) klass)))

         (let ((code (emit-<class> class ldk-loader)))
           ;; In AOT mode, also write the class definitions out
           (when *aot-dir*
             (%write-aot-class classname code))
           (%eval code))
         (%install-method-stubs class ldk-loader)

         ;; Record this class's MEMBER classes (Class.getDeclaredClasses):
         ;; InnerClasses entries whose outer class is this class.  Entries
         ;; with outer index 0 are local/anonymous classes, which are not
         ;; declared members.
         (dolist (ic (gethash "InnerClasses" (attributes class)))
           (unless (zerop (outer-class-info-index ic))
             (let* ((outer-ref (aref (constant-pool class) (outer-class-info-index ic)))
                    (outer-name (slot-value (aref (constant-pool class) (index outer-ref)) 'value)))
               (when (string= outer-name (name class))
                 (let ((inner-ref (aref (constant-pool class) (inner-class-info-index ic))))
                   (push (aref (constant-pool class) (index inner-ref))
                         (inner-classes class)))))))

         ;; Populate NestHost/NestMembers from class attributes (JDK 11+)
         (when-let ((host-index (gethash "NestHost" (attributes class))))
           (let* ((class-ref (aref (constant-pool class) host-index))
                  (name-entry (aref (constant-pool class) (index class-ref))))
             (setf (nest-host class) (slot-value name-entry 'value))))
         (when-let ((member-indices (gethash "NestMembers" (attributes class))))
           (setf (nest-members class)
                 (mapcar (lambda (idx)
                           (let* ((class-ref (aref (constant-pool class) idx))
                                  (name-entry (aref (constant-pool class) (index class-ref))))
                             (slot-value name-entry 'value)))
                         member-indices)))

         ;; Emit the class initializer
         (let ((lisp-class (find-class classname-symbol)))
           (closer-mop:finalize-inheritance lisp-class)
           (let ((icc (append (list 'defun (intern (format nil "%clinit-~A" classname) pkg) (list))
                              (loop for k in (reverse (closer-mop:class-precedence-list lisp-class))
                                    for k-name = (format nil "~A" (class-name k))
                                    for k-ldk-class = (%get-ldk-class-by-bin-name k-name t ldk-loader)
                                    ;; Each superclass's clinit is in its defining loader's package
                                    for k-pkg = (if k-ldk-class
                                                    (loader-package (slot-value k-ldk-class 'ldk-loader))
                                                    pkg)
                                    for clinit-function = (intern (format nil "~a.<clinit>()V" (class-name k)) k-pkg)
                                    ;; JLS 12.4.2: initialize this class, its superclasses, and
                                    ;; only those superinterfaces that declare a default method --
                                    ;; NOT every superinterface in the precedence list.
                                    when (and k-ldk-class (fboundp clinit-function)
                                              (or (string= k-name classname)
                                                  (not (interface-p k-ldk-class))
                                                  (%declares-default-method-p k-ldk-class)))
                                      collect (list '%ensure-class-initialized
                                                    k-ldk-class
                                                    (list 'function clinit-function))))))
             (%evaluate-java-definition class icc)))

         ;; Check if this is a Throwable subclass - find Throwable in its defining package
         (let ((throwable-symbol (intern "java/lang/Throwable" (class-package "java/lang/Throwable"))))
           (when (and (not (string= classname "java/lang/Throwable"))
                      (find-class throwable-symbol nil)  ;; Make sure Throwable is loaded
                      (subtypep classname-symbol (find-class throwable-symbol)))
             ;; Condition symbols always in :openldk for cross-loader exception catching
             (let* ((condition-symbol (intern (format nil "condition-~A" classname) :openldk))
                    (parent-condition-symbol (intern (format nil "condition-~A" (slot-value super 'name)) :openldk)))
               (setf (gethash (find-class classname-symbol) *condition-table*) condition-symbol)
               (let ((ccode `(define-condition ,condition-symbol (,parent-condition-symbol)
                               ())))
                 (%eval ccode))
               ;; %lisp-condition method: class specializer in loader's package
               (let ((ccode `(defmethod %lisp-condition ((throwable ,classname-symbol))
                               (let ((c (make-condition (quote ,condition-symbol))))
                                 ;; Debug: print backtrace for Error types
                                 (when (and *debug-codegen* (search "Error" ,classname))
                                   (format t "~%; DEBUG: Creating ~A~%" ,classname)
                                   (trivial-backtrace:print-backtrace c :output *standard-output*))
                                 (setf (slot-value c '|objref|) throwable)
                                 c))))
                 (%eval ccode)))))

         ;; Load all of the field classes
         (loop for field across (fields class)
               do (classload (slot-value (slot-value field 'class) 'name)))

         (setf completed t)
         class)
      (close classfile-stream)
      (when (and definition (not completed))
        (%forget-failed-class-definition definition ldk-loader)))))

(defun %classload-system (classname)
  "Load CLASSNAME as the boot or app class loader would, from the JDK and
the classpath, or return NIL.  CLASSLOAD also consults the global by-name
table, which can hold a class of this name defined by an unrelated user
loader; that is never the answer here."
  (let ((class (classload classname)))
    (when (and class
               (not (%user-ldk-loader-p (and (slot-boundp class 'ldk-loader)
                                             (slot-value class 'ldk-loader)))))
      class)))

(defun %stub-class-reference (class)
  "How a method stub names CLASS to %COMPILE-METHOD: the <class> itself, or
its name in AOT output, which is written out as Lisp source."
  (if *aot-dir* (slot-value class 'name) (%weak-class-reference class)))

(defvar *classes-being-resolved* nil
  "(LDK-LOADER . CLASSNAME) pairs this thread is resolving through a Java
class loader, to stop a resolution cycle from recursing forever.")

(defvar *loaders-overriding-load-class* (make-hash-table :test #'eq :weakness :key :synchronized t)
  "CLOS class of a Java class loader -> whether it overrides loadClass.")

(defun %overrides-load-class-p (java-loader)
  "True when JAVA-LOADER's class, or a superclass below the JDK's, declares
loadClass: a child-first loader, which may define a class itself that its
parent could also provide."
  (let ((clos-class (class-of java-loader)))
    (multiple-value-bind (overrides found) (gethash clos-class *loaders-overriding-load-class*)
      (if found
          overrides
          (setf (gethash clos-class *loaders-overriding-load-class*)
                (loop with class = (get-ldk-class-for-java-class (|getClass()Ljava/lang/Class;| java-loader))
                      while (and class (not (jdk-class-p (name class))))
                      thereis (some (lambda (m) (and m (string= (slot-value m 'name) "loadClass")))
                                    (coerce (methods class) 'list))
                      do (setf class (and (super class)
                                          (%get-ldk-class-by-bin-name (super class) t
                                                                      (ldk-loader class))))))))))

(defun %resolve-class-without-initiation (classname ldk-loader)
  "Resolve CLASSNAME through its defining LDK-LOADER, or return NIL.
A user loader's loadClass implements its complete delegation policy; the
application classpath must not be searched on that loader's behalf."
  (let* ((classname (coerce classname 'string))
         (java-loader (and ldk-loader (slot-value ldk-loader 'java-loader)))
         (askable (and java-loader
                       (%user-ldk-loader-p ldk-loader)
                       (not (eql (char classname 0) #\[)))))
    (flet ((ask-java-loader ()
             (let ((key (cons ldk-loader classname)))
               (unless (member key *classes-being-resolved* :test #'equal)
                 (let ((*classes-being-resolved* (cons key *classes-being-resolved*))
                       ;; The Java code loadClass runs must not see the
                       ;; context of a method compilation in progress.
                       (*context* nil))
                   (handler-case
                       (let ((java-class (|loadClass(Ljava/lang/String;)Ljava/lang/Class;|
                                          java-loader
                                          (jstring (substitute #\. #\/ classname)))))
                         (when java-class
                           (get-ldk-class-for-java-class java-class)))
                     (|condition-java/lang/ClassNotFoundException| () nil)))))))
      (if askable
          ;; Only this loader's own definitions may bypass its delegation
          ;; policy. A null-parent loader must not see the app classpath.
          (or (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))
              (ask-java-loader))
          (or (and ldk-loader (%get-ldk-class-by-bin-name classname t ldk-loader))
              (%classload-system classname))))))

(defun %resolve-class (classname ldk-loader)
  "Resolve through a stable initiating-loader identity, recording delegated results."
  (or (%initiated-class classname ldk-loader)
      (when-let ((class (%resolve-class-without-initiation classname ldk-loader)))
        (%record-initiating-class class (or ldk-loader (ldk-loader class))))))

(defun %resolve-class-in-context (classname)
  "Resolve CLASSNAME for the method being compiled: through the loader of
the class being compiled when there is one, as the JVM resolves references."
  (%resolve-class classname (and *context* (ldk-loader *context*))))

(defun classload (classname &optional (class-loader nil))
  "Load a class from the classpath using proper JVM class loader delegation.
   JDK classes (java/*, javax/*, sun/*, etc.) are loaded by boot loader.
   User classes are loaded by app loader (which delegates to boot loader first).
   CLASS-LOADER is the java.lang.ClassLoader object (usually NIL for system loader)."
  (let ((classname (coerce classname 'string)))
    (assert (not (find #\. classname)))
    (assert (> (length classname) 0))
    (%note-class-dependency classname)
    (cond
      ;; During warm-up (before main initializes loaders), use global tables
      ((not *boot-ldk-class-loader*)
       (let ((class (gethash classname *ldk-classes-by-bin-name*)))
         (if class
             class
             (bordeaux-threads:with-recursive-lock-held ((%get-class-lock classname))
               ;; Re-check after acquiring lock
               (or (gethash classname *ldk-classes-by-bin-name*)
                   (let ((classfile-stream (open-java-classfile-on-classpath classname)))
                     (if classfile-stream
                         (progn
                           (when *debug-load*
                             (format t "~&; LOADING   ~A~%" classname))
                           ;; Create temporary boot loader for warm-up
                           (let* ((system-pkg (or (find-package "OPENLDK.SYSTEM")
                                                  (make-package "OPENLDK.SYSTEM" :use '(:openldk))))
                                  (temp-loader (make-instance '<ldk-class-loader>
                                                              :id 0
                                                              :pkg system-pkg
                                                              :parent-loader nil
                                                              :java-loader nil
                                                              :ldk-classes-by-bin-name *ldk-classes-by-bin-name*
                                                              :ldk-classes-by-fq-name *ldk-classes-by-fq-name*
                                                              :java-classes-by-bin-name *java-classes-by-bin-name*
                                                              :java-classes-by-fq-name *java-classes-by-fq-name*)))
                             (%classload-from-stream classname classfile-stream class-loader temp-loader)))
                         nil)))))))
      ;; Check if already loaded by boot loader, or during warm-up before the
      ;; loaders were created.  The boot loader's table is the global table,
      ;; which also holds user loaders' classes: those are not the system
      ;; loaders' to hand out, and a same-named class may be on the classpath.
      ((let ((class (gethash classname *ldk-classes-by-bin-name*)))
         (and class
              (not (%defined-by-user-loader-p class 'ldk-classes-by-bin-name))
              class)))
      ;; JDK classes always loaded by boot loader (into :openldk)
      ((jdk-class-p classname)
       (bordeaux-threads:with-recursive-lock-held ((%get-class-lock classname))
         ;; Re-check after acquiring lock
         (or (gethash classname (slot-value *boot-ldk-class-loader* 'ldk-classes-by-bin-name))
             (gethash classname *ldk-classes-by-bin-name*)
             (let ((classfile-stream (open-java-classfile-on-classpath classname)))
               (when classfile-stream
                 (when *debug-load*
                   (format t "~&; LOADING   ~A~%" classname))
                 (%classload-from-stream classname classfile-stream class-loader *boot-ldk-class-loader*))))))
      ;; User classes loaded by app loader (into OPENLDK.APP)
      (*app-ldk-class-loader*
       ;; First check if already loaded by app loader
       (or (gethash classname (slot-value *app-ldk-class-loader* 'ldk-classes-by-bin-name))
           (bordeaux-threads:with-recursive-lock-held ((%get-class-lock classname))
             ;; Re-check after acquiring lock
             (or (gethash classname (slot-value *app-ldk-class-loader* 'ldk-classes-by-bin-name))
                 (let ((classfile-stream (open-java-classfile-on-classpath classname)))
                   (when classfile-stream
                     (when *debug-load*
                       (format t "~&; LOADING   ~A~%" classname))
                     (%classload-from-stream classname classfile-stream class-loader *app-ldk-class-loader*)))))))
      ;; Fallback to boot loader if app loader not yet initialized
      (t
       (bordeaux-threads:with-recursive-lock-held ((%get-class-lock classname))
         ;; Re-check after acquiring lock
         (or (gethash classname *ldk-classes-by-bin-name*)
             (let ((classfile-stream (open-java-classfile-on-classpath classname)))
               (when classfile-stream
                 (when *debug-load*
                   (format t "~&; LOADING   ~A~%" classname))
                 (%classload-from-stream classname classfile-stream class-loader *boot-ldk-class-loader*)))))))))

(defun %java-home-major-version (java-home)
  "Parse the JDK major version from $JAVA_HOME/release, or NIL if unknown."
  (let ((release (merge-pathnames "release" (uiop:ensure-directory-pathname java-home))))
    (when (uiop:file-exists-p release)
      (with-open-file (in release :if-does-not-exist nil)
        (when in
          (loop for line = (read-line in nil)
                while line
                when (str:starts-with? "JAVA_VERSION=" line)
                  return (let* ((q1 (position #\" line))
                                (q2 (and q1 (position #\" line :start (1+ q1))))
                                (version (and q2 (subseq line (1+ q1) q2))))
                           (when version
                             (parse-integer version
                                            :end (position #\. version)
                                            :junk-allowed t)))))))))

(defun %jdk-25-home-p (java-home)
  "True if JAVA-HOME is a JDK 25 with runtime classes OpenLDK can read."
  (let ((home (uiop:ensure-directory-pathname java-home)))
    (and (eql 25 (%java-home-major-version home))
         (or (directory (merge-pathnames "jmods/*.jmod" home))
             (uiop:file-exists-p (merge-pathnames "lib/modules" home))))))

(defun %java-on-path-home ()
  "The JDK home that the first `java' on PATH belongs to, if any."
  (dolist (dir (uiop:split-string (or (uiop:getenv "PATH") "")
                                  :separator (string (uiop:inter-directory-separator))))
    (when (plusp (length dir))
      (let ((java (uiop:file-exists-p
                   (merge-pathnames "java" (uiop:ensure-directory-pathname dir)))))
        (when java
          ;; Follow the /usr/bin/java -> /etc/alternatives -> ... links
          ;; to <home>/bin/java.
          (return (uiop:pathname-parent-directory-pathname
                   (uiop:pathname-directory-pathname (truename java)))))))))

(defun find-installed-jdk ()
  "Look for a JDK 25 in the places JDKs are commonly installed."
  (let ((home (user-homedir-pathname)))
    (flet ((matches (pattern &optional (base #p"/"))
             (sort (mapcar #'namestring
                           (directory (merge-pathnames pattern base) :resolve-symlinks nil))
                   #'string>)))
      (find-if #'%jdk-25-home-p
               (remove nil
                       (list* (%java-on-path-home)
                              (append
                               (matches "usr/lib/jvm/*/")
                               (matches "usr/java/*/")
                               (matches "opt/*/")
                               (matches "opt/java/*/")
                               (matches ".sdkman/candidates/java/*/" home)
                               (matches ".jdks/*/" home)
                               (matches ".asdf/installs/java/*/" home)
                               (matches ".local/share/mise/installs/java/*/" home)
                               (matches "Library/Java/JavaVirtualMachines/*/Contents/Home/")
                               (matches "Library/Java/JavaVirtualMachines/*/Contents/Home/" home))))))))

(defun ensure-java-home ()
  (let* ((JAVA_HOME (uiop:getenvp "JAVA_HOME"))
         (major (and JAVA_HOME (%java-home-major-version JAVA_HOME))))
    ;; No JDK 25 given: use an installed one, or else download our own,
    ;; and point JAVA_HOME at it so everything that reads the variable
    ;; finds it.
    (when (or (null JAVA_HOME) (and major (/= major 25)))
      (handler-case
          (setf JAVA_HOME (string-right-trim "/" (uiop:native-namestring
                                                  (or (find-installed-jdk) (ensure-cached-jdk))))
                (uiop:getenv "JAVA_HOME") JAVA_HOME)
        (fetch-error (e)
          (if JAVA_HOME
              (format *error-output* "~%OpenLDK Error: JAVA_HOME points at a JDK ~A installation:~%  ~A~%" major JAVA_HOME)
              (format *error-output* "~%OpenLDK Error: JAVA_HOME environment variable not set~%"))
          (format *error-output* "  and no JDK 25 is installed in the usual places, nor could one be downloaded:~%  ~A~%" e)
          (format *error-output* "  Set JAVA_HOME to a JDK 25 installation (e.g. /usr/lib/jvm/java-25-openjdk).~%")
          (uiop:quit 1))))

    (cond
      ((uiop:directory-exists-p (concatenate 'string JAVA_HOME "/jmods/"))
       (unless (directory (merge-pathnames "*.jmod" (concatenate 'string JAVA_HOME "/jmods/")))
         (format *error-output* "~%OpenLDK Error: No .jmod files found in $JAVA_HOME/jmods/~%")
         (uiop:quit 1)))
      ;; Headless JDK builds ship no jmods/ directory; the runtime classes live
      ;; in the jimage container at $JAVA_HOME/lib/modules instead.
      ((uiop:file-exists-p (concatenate 'string JAVA_HOME "/lib/modules")))
      (t
       (format *error-output* "~%OpenLDK Error: Cannot find $JAVA_HOME/jmods/ or $JAVA_HOME/lib/modules~%")
       (format *error-output* "  OpenLDK requires JDK 25. Set JAVA_HOME to a JDK 25 installation.~%")
       (uiop:quit 1)))))

(defun %thread-daemon-p (thread)
  "Check if a Java Thread is a daemon thread.
   JDK 21: isDaemon() reads from holder.daemon, so check holder first."
  (cond
    ;; JDK 21: daemon flag lives in Thread$FieldHolder
    ((and (slot-exists-p thread '|holder|)
          (ignore-errors (slot-boundp thread '|holder|))
          (slot-value thread '|holder|))
     (let ((holder (slot-value thread '|holder|)))
       (and (slot-exists-p holder '|daemon|)
            (ignore-errors (slot-boundp holder '|daemon|))
            (not (zerop (or (slot-value holder '|daemon|) 0))))))
    ;; Fallback: direct daemon field (JDK 17 and earlier)
    ((and (slot-exists-p thread '|daemon|)
          (ignore-errors (slot-boundp thread '|daemon|)))
     (not (zerop (or (slot-value thread '|daemon|) 0))))
    (t nil)))

(defun %parse-ldk-debug-flags ()
  "Set the *debug-...* flags from the LDK_DEBUG environment variable."
  (let ((LDK_DEBUG (uiop:getenv "LDK_DEBUG")))
    (when LDK_DEBUG
      (when (find #\c LDK_DEBUG) (setf *debug-codegen* t))
      (when (find #\l LDK_DEBUG) (setf *debug-load* t))
      (when (find #\L LDK_DEBUG) (setf *debug-load* t) (setf *debug-compile* t))
      (when (find #\s LDK_DEBUG) (setf *debug-slynk* t))
      (when (find #\t LDK_DEBUG) (setf *debug-trace* t))
      (when (find #\T LDK_DEBUG) (setf *debug-trace-args* t))
      (when (find #\b LDK_DEBUG) (setf *debug-bytecode* t))
      (when (find #\e LDK_DEBUG) (setf *debug-exceptions* t))
      (when (find #\x LDK_DEBUG) (setf *debug-x* t))
      (when (find #\u LDK_DEBUG) (setf *debug-unmuffle* t))
      (when (find #\p LDK_DEBUG) (setf *debug-propagation* t)))))

(defun %ensure-system-loader-packages ()
  "Ensure the system ClassLoader's `packages` map exists.  ClassLoader.<init>
initializes `packages` (a ConcurrentHashMap getDefinedPackage reads) inline,
but when that constructor aborts on the registerAsParallelCapable
InternalError %INITIALIZE-SYSTEM-PHASE1 swallows, the field is left null and
every getPackage/getDefinedPackage on the loader throws NullPointerException.
In the polyglot image that broke Kawa's symbol resolution (gnu.bytecode's
getContextPackage) and Scala's engine startup alike.  Run on every start,
build and restart, so a dumped image whose loader lost the map recovers it."
  (when (and *boot-class-loader*
             (slot-exists-p *boot-class-loader* '|packages|)
             (null (slot-value *boot-class-loader* '|packages|)))
    (classload "java/util/concurrent/ConcurrentHashMap")
    (let ((map (%make-java-instance "java/util/concurrent/ConcurrentHashMap")))
      (|<init>()V| map)
      (setf (slot-value *boot-class-loader* '|packages|) map))))

(defun %main-runtime-setup (classpath dump-dir aot)
  "Per-run setup shared by every MAIN invocation: Java FP semantics, the
VM start-time stamp, SIGQUIT handler, class loaders, debug flags, system
properties, and *CLASSPATH* construction."
  ;; Java floating-point semantics: float division by zero must yield
  ;; infinity/NaN, not trap (javac itself relies on 1.0/0.0 in isPosZero).
  ;; The image-entry wrappers also set this, but MAIN can be called
  ;; directly (build-time warmups, the REPL).
  (sb-int:set-floating-point-modes :traps nil)
  ;; Stamp the VM start time (RuntimeMXBean.getStartTime) on first entry.
  (unless *vm-start-time-millis*
    (setf *vm-start-time-millis* (|java/lang/System.currentTimeMillis()J|)))
  (ensure-java-home)
  ;; Install SIGQUIT handler for debugging hangs (send kill -3 to dump all stacks)
  (install-sigquit-handler)
  (install-dump-watchdog)
  ;; Boot class loader: OPENLDK.SYSTEM package, JDK classes only.
  (unless *boot-ldk-class-loader*
    (let ((system-pkg (or (find-package "OPENLDK.SYSTEM")
                          (make-package "OPENLDK.SYSTEM" :use '(:openldk)))))
      (setf *boot-ldk-class-loader*
            (make-instance '<ldk-class-loader>
                           :id 0
                           :pkg system-pkg
                           :parent-loader nil
                           :java-loader nil
                           :ldk-classes-by-bin-name *ldk-classes-by-bin-name*
                           :ldk-classes-by-fq-name *ldk-classes-by-fq-name*
                           :java-classes-by-bin-name *java-classes-by-bin-name*
                           :java-classes-by-fq-name *java-classes-by-fq-name*))))
  (setf *module-access-ready* nil)
  ;; Application class loader: OPENLDK.APP package, child of boot.
  (unless *app-ldk-class-loader*
    (setf *app-ldk-class-loader*
          (make-ldk-class-loader :parent-loader *boot-ldk-class-loader*
                                 :java-loader nil
                                 :package-name "OPENLDK.APP")))
  ;; The system class loader (getSystemClassLoader) is the application
  ;; classes' loader, as on a JVM.
  (when *boot-class-loader*
    (setf (gethash *boot-class-loader* *java-to-ldk-loaders*) *app-ldk-class-loader*))
  (%ensure-system-loader-packages)
  (%enable-class-unloading)
  (%parse-ldk-debug-flags)
  ;; Enable DCE via environment variable LDK_DCE=1 (or any non-empty value)
  (let ((LDK_DCE (uiop:getenv "LDK_DCE")))
    (when (and LDK_DCE (plusp (length LDK_DCE)))
      (setf *enable-dce* t)))
  ;; Reset system properties to fix things that change between
  ;; build-time and run-time.
  (|java/lang/System.initProperties(Ljava/util/Properties;)|
   (slot-value |+static-java/lang/System+| '|props|))
  ;; Apply -D system properties from command line
  (dolist (prop *cli-jvm-properties*)
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|
     (ijstring (car prop)) (ijstring (cdr prop))))
  (%configure-runtime-encoding)
  (when *debug-slynk*
    (slynk:create-server :port 2025)
    (sleep 10))
  (setf *dump-dir* dump-dir)
  (setf *aot-dir* aot)
  (let ((classpath (or classpath
                       (uiop:getenv "CLASSPATH")
                       (uiop:getenv "LDK_CLASSPATH")
                       ".")))
    ;; java.class.path is the classpath used, given as an argument (-cp,
    ;; openldk.interop:start-runtime) too: Scala's compiler reads it.
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|
     (ijstring "java.class.path") (ijstring classpath))
    (setf *classpath*
          (append
           (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
                 collect (if (ends-with? ".jar" cpe)
                             (make-instance 'jar-classpath-entry :jarfile cpe)
                             (make-instance 'dir-classpath-entry :dir cpe)))
           (discover-jmod-classpath-entries))))
  (when *cli-add-modules* (%reset-boot-module-layer))
  (loop for modules in (reverse *cli-add-modules*)
        for index from 0
        do (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;|
            (jstring (format nil "jdk.module.addmods.~D" index)) (jstring modules)))
  ;; Resolve the boot graph before application loading/JIT compilation can
  ;; reenter module initialization through an access check.
  (%boot-module-layer)
  (%configure-native-access)
  (setf *module-access-ready* t))

(defun %main-method-declared-by (class descriptor public-only &optional inherited-interface)
  "Find a candidate main declared by CLASS for DESCRIPTOR."
  (find-if (lambda (method)
             (and (string= (name method) "main")
                  (string= (descriptor method) descriptor)
                  (not (logbitp 1 (access-flags method)))
                  (or (not public-only) (logbitp 0 (access-flags method)))
                  (not (and inherited-interface (static-p method)))))
           (methods class)))

(defun %main-interface-extends-p (child parent)
  "Whether interface CHILD extends PARENT, including transitively."
  (or (eq child parent)
      (some (lambda (name)
              (when-let ((interface (%resolve-class name (ldk-loader child))))
                (%main-interface-extends-p interface parent)))
            (coerce (interfaces child) 'list))))

(defun %find-main-method (class descriptor public-only)
  "Return (declaring-class . method), searching classes before interfaces.
Static interface methods are not inherited; subinterface declarations take
precedence over declarations of their superinterfaces."
  (let ((interface-candidates nil)
        (seen nil))
    (labels ((visit-interface (interface)
               (unless (member interface seen)
                 (push interface seen)
                 (let ((method (%main-method-declared-by interface descriptor public-only t)))
                   (if method
                       (push (cons interface method) interface-candidates)
                       (map nil (lambda (name)
                                  (when-let ((parent (%resolve-class name (ldk-loader interface))))
                                    (visit-interface parent)))
                            (interfaces interface)))))))
      (loop for current = class then (and (super current)
                                         (%resolve-class (super current) (ldk-loader current)))
            while current
            do (when-let ((method (%main-method-declared-by current descriptor public-only)))
                 (return-from %find-main-method (cons current method)))
               (map nil (lambda (name)
                          (when-let ((interface (%resolve-class name (ldk-loader current))))
                            (visit-interface interface)))
                    (interfaces current)))
      (find-if (lambda (candidate)
                 (notany (lambda (other)
                           (and (not (eq candidate other))
                                (%main-interface-extends-p (car other) (car candidate))))
                         interface-candidates))
               interface-candidates))))

(defun %invoke-java-main (class argv)
  "Select and invoke a Java 25 main, constructing the launched class if needed."
  (let* ((candidate (or (%find-main-method class "([Ljava/lang/String;)V" t)
                        (%find-main-method class "([Ljava/lang/String;)V" nil)
                        (%find-main-method class "()V" nil))))
    (unless candidate
      (error "Main method not found in class ~A." (name class)))
    (let* ((owner (car candidate))
           (method (cdr candidate))
           (args (unless (string= (descriptor method) "()V") (list argv))))
      (if (static-p method)
          (%eval (cons (static-method-symbol
                        (format nil "~A.main~A" (name owner) (descriptor method))
                        (ldk-class-package owner)) args))
          (let ((constructor (find-if (lambda (method)
                                        (and (string= (name method) "<init>")
                                             (string= (descriptor method) "()V")
                                             (not (logbitp 1 (access-flags method)))))
                                      (methods class))))
            (when (or (interface-p class) (abstract-p class) (null constructor))
              (error "Instance main in ~A requires a concrete class with a non-private no-argument constructor."
                     (name class)))
            (let ((instance (%make-java-instance (name class) (ldk-loader class))))
              (invoke-special '|<init>()V| (intern (name class) (ldk-class-package class)) (list instance))
              (%eval (list* (intern (format nil "main~A" (descriptor method)) :openldk)
                            instance args))))))))

(defun %await-nondaemon-java-threads ()
  "Wait for all non-daemon Java threads (other than the current one) to
complete, as a JVM does.  LDK_THREAD_WAIT_SECONDS=N gives up after N
seconds instead."
  (let ((current-lisp-thread (bordeaux-threads:current-thread))
        (deadline
          (let* ((setting (uiop:getenv "LDK_THREAD_WAIT_SECONDS"))
                 (seconds (and setting
                               (parse-integer setting :junk-allowed t))))
            (when (and seconds (plusp seconds))
              (+ (get-internal-real-time)
                 (* seconds internal-time-units-per-second))))))
    (loop
      (let ((platform-threads
              ;; start0 records this mapping in the parent after
              ;; MAKE-THREAD returns.  The reverse mapping is
              ;; installed by the child and can therefore still be
              ;; empty when a short Java main method returns.
              (loop for java-thread being the hash-keys of *java-threads*
                      using (hash-value lisp-thread)
                    when (and (not (eq lisp-thread current-lisp-thread))
                              (not (%thread-daemon-p java-thread))
                              (bordeaux-threads:thread-alive-p lisp-thread))
                      collect java-thread))
            #+sb-fiber
            (fiber-threads
              (loop for java-thread being the hash-values of *fiber-to-java-threads*
                    when (and (not (%thread-daemon-p java-thread))
                              (let ((fiber (gethash java-thread *java-to-fibers*)))
                                (and fiber (uiop:symbol-call :sb-thread :fiber-alive-p fiber))))
                      collect java-thread))
            #-sb-fiber
            (fiber-threads nil))
        (let ((java-threads (append platform-threads fiber-threads (%jni-nondaemon-threads))))
          (cond
            ((null java-threads)
             ;; No sleep first: every non-daemon thread has ended, so what
             ;; they wrote is in the stream already.
             (finish-output)
             (return))
            ((and deadline (> (get-internal-real-time) deadline))
             (finish-output)
             (return))
            (t
             (sleep 0.1))))))))

(defun main (mainclass &optional (args (list)) &key dump-dir classpath aot)
  "Run a Java class with the given arguments.
   MAINCLASS: The class or interface whose Java main method is executed.
   ARGS: Java program command line arguments (list of strings).
   CLASSPATH: The classpath from which classes are loaded.
   DUMP-DIR: The directory into which internal debug info is dumped.
   AOT: Ahead-of-time compilation directory (generate Lisp source files)."
  (%main-runtime-setup classpath dump-dir aot)
  (when (plusp (hash-table-count *java-references*))
    (%ensure-java-reference-processor))
  (if *aot-dir*
      (%run-aot-main mainclass aot)
      (let* ((class (classload (substitute #\/ #\. mainclass)))
             (argv (make-java-array
                    :component-class (%get-java-class-by-bin-name "java/lang/String")
                    :initial-contents (mapcar #'jstring args))))
        (assert (or class (error "Can't load ~A" mainclass)))
        (%clinit class)
        (%invoke-java-main class argv)
        (%await-nondaemon-java-threads))))


(defun %java-string (value)
  (cond
    ((null value) "")
    ((typep value '|java/lang/String|) (lstring value))
    ((stringp value) value)
    (t (format nil "~A" value))))

(defun %print-java-stack-trace (throwable &key (stream *error-output*) (indent 0) (prefix nil) (visited (make-hash-table :test #'eq)))
  (when throwable
    (unless (gethash throwable visited)
      (setf (gethash throwable visited) t)
      (let* ((indent-str (make-string indent :initial-element #\space))
             (header (%java-string (|toString()Ljava/lang/String;| throwable))))
        (if prefix
            (format stream "~&~A~A~A~%" indent-str prefix header)
            (format stream "~&~A~A~%" indent-str header))
        (when (and (slot-boundp throwable '|backtrace|)
                   (slot-value throwable '|backtrace|))
          (let ((depth (|getStackTraceDepth()| throwable)))
            (dotimes (i depth)
              (let* ((ste (|getStackTraceElement(I)| throwable i))
                     (line (%java-string (|toString()Ljava/lang/String;| ste))))
                (format stream "~&~A    at ~A~%" indent-str line))))))
      (let ((cause (cond
                     ((and (slot-exists-p throwable '|cause|)
                           (slot-boundp throwable '|cause|)
                           (slot-value throwable '|cause|))
                      (slot-value throwable '|cause|))
                     ((and (slot-exists-p throwable '|exception|)
                           (slot-boundp throwable '|exception|)
                           (slot-value throwable '|exception|))
                      (slot-value throwable '|exception|)))))
        (when cause
          (%print-java-stack-trace cause
                                   :stream stream
                                   :indent indent
                                   :prefix "Caused by: "
                                   :visited visited))))))

(defun %initialize-classpath ()
  "Build *CLASSPATH* from the LDK_CLASSPATH environment variable plus
the discovered jmod entries."
  (let ((classpath (or (uiop:getenv "LDK_CLASSPATH") "")))

    (setf *classpath*
          (append
           (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
                 when (plusp (length cpe))
                 collect (if (ends-with? ".jar" cpe)
                             (make-instance 'jar-classpath-entry :jarfile cpe)
                             (make-instance 'dir-classpath-entry :dir cpe)))
           (discover-jmod-classpath-entries)))))

(defun %initialize-bootstrap-classes ()
  "Hand-load the classes Class.forName0 itself depends on, then patch
jdk.internal.misc.UnsafeConstants with the real platform values (its
<clinit> deliberately zeroes them, expecting the JVM to fill them in)."
  ;; We need to hand load these before Class.forName0 will work.
  (%clinit (classload "java/lang/Object"))
  (%clinit (classload "java/lang/String"))
  (%clinit (classload "java/lang/Class"))
  (%clinit (classload "java/lang/ClassLoader"))

  ;; UnsafeConstants fields are normally pre-populated by the JVM
  ;; before <clinit> runs (which deliberately sets them all to 0).
  ;; We must set the real values after <clinit>.
  (let ((uc (classload "jdk/internal/misc/UnsafeConstants")))
    (%clinit uc)
    (let* ((pkg (or (find-package "OPENLDK.SYSTEM")
                    (make-package "OPENLDK.SYSTEM" :use '(:openldk))))
           (static-sym (find-symbol (format nil "+static-~A+" (name uc)) pkg)))
      (when (and static-sym (boundp static-sym))
        (let ((s (symbol-value static-sym)))
          (setf (slot-value s '|ADDRESS_SIZE0|) 8)
          (setf (slot-value s '|PAGE_SIZE|) (sb-posix:getpagesize))
          (setf (slot-value s '|BIG_ENDIAN|) 0)
          (setf (slot-value s '|UNALIGNED_ACCESS|) 1)
          (setf (slot-value s '|DATA_CACHE_LINE_FLUSH_SIZE|) 0))))))

(defun %initialize-primitive-classes ()
  "Synthesize java.lang.Class objects for the primitive types."
  (dolist (p '(("byte" . "B") ("char" . "C") ("int" . "I")
               ("short" . "S") ("long" . "J") ("double" . "D")
               ("float" . "F") ("boolean" . "Z") ("void" . "V")))
    (let ((jclass (%make-java-instance "java/lang/Class"))
          (lclass (make-instance '<class>)))
      (setf (slot-value jclass '|name|) (ijstring (car p)))
      ;; JDK 25: java.lang.Class.isPrimitive() reads the boolean `primitive`
      ;; field directly (it is no longer a native method), so synthetic
      ;; primitive Class objects must have it set.
      (when (slot-exists-p jclass '|primitive|)
        (setf (slot-value jclass '|primitive|) 1))
      (setf (name lclass) (car p))
      (setf (java-class lclass) jclass)
      (setf (gethash (car p) *ldk-classes-by-fq-name*) lclass)
      (setf (gethash (car p) *ldk-classes-by-bin-name*) lclass)
      (setf (gethash (car p) *java-classes-by-fq-name*) jclass)
      (setf (gethash (car p) *java-classes-by-bin-name*) jclass))))

(defun %preload-early-classes (boot-class-loader)
  "Preload classes needed before System.initPhase1 can run."
  ;; Preload some important classes.
  (dolist (c '("java/lang/Boolean"
               "java/lang/Character"
               "java/lang/Byte"
               "java/lang/Short"
               "java/lang/Integer"
               "java/lang/Long"
               "java/lang/Float"
               "java/lang/Double"
               "java/nio/LongBuffer"
               "java/lang/Void"
               "java/lang/ClassLoader"
               "java/security/PrivilegedAction"
               "java/lang/StackTraceElement"
               "java/lang/System"
               "java/lang/ThreadGroup"
               "java/lang/Thread"
               "java/lang/ref/SoftReference"
               "java/util/Properties"
               "java/lang/SecurityManager"))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (jstring c) nil boot-class-loader nil)))

(defun %seed-system-properties (property-alist)
  "Create the System props table and seed essential properties needed
during early JDK init (many JDK components assume non-NIL encodings),
plus user.* properties and the caller-supplied PROPERTY-ALIST."
  (let ((props (%make-java-instance "java/util/Properties")))
    (|<init>()V| props)
    (setf (slot-value |+static-java/lang/System+| '|props|) props))

  ;; Seed essential system properties needed during early JDK init.
  ;; Many JDK components assume non-NIL encodings.
  (dolist (kv `(("file.encoding" . "UTF-8")
                ("sun.jnu.encoding" . "UTF-8")
                ("sun.stdout.encoding" . "UTF-8")
                ("sun.stderr.encoding" . "UTF-8")
                ("line.separator" . "\n")
                ("file.separator" . "/")
                ("path.separator" . ":")
                ("os.name" . "Linux")
                ("os.arch" . "amd64")
                ("os.version" . "")
                ("java.io.tmpdir" . "/tmp")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;| (ijstring (car kv)) (ijstring (cdr kv))))

  ;; Also set java.home for code that queries it early.
  (when-let ((jh (uiop:getenv "JAVA_HOME")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;| (ijstring "java.home") (ijstring jh)))

  ;; Populate common user properties
  (when-let ((cwd (uiop:getcwd)))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;| (ijstring "user.dir") (ijstring (namestring cwd))))
  (when-let ((uh (or (uiop:getenv "HOME") "/")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;| (ijstring "user.home") (ijstring uh)))
  (when-let ((un (or (uiop:getenv "USER") (uiop:getenv "LOGNAME") "openldk")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;| (ijstring "user.name") (ijstring un)))

  ;; Add user-provided properties...
  (dolist (prop property-alist)
    (assert (typep prop 'list))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)Ljava/lang/String;| (ijstring (car prop)) (ijstring (cdr prop)))))

(defun %initialize-system-phase1 (boot-class-loader)
  "Run System.initPhase1 and the module/class-loader setup around it."
  ;; Ensure AccessibleObject <clinit> runs before initPhase1,
  ;; because ReflectionFactory reads langReflectAccess from SharedSecrets
  ;; in its constructor, and AccessibleObject <clinit> is what sets it.
  (%clinit (classload "java/lang/reflect/AccessibleObject"))

  ;; Call System.initPhase1() which sets up JavaLangAccess (JLA),
  ;; system properties (via SystemProps$Raw native methods), I/O streams
  ;; (System.in/out/err), and VM.initLevel(1).
  (|java/lang/System.initPhase1()V|)

  ;; Create an unnamed Module for bootstrap classes.
  ;; Class.getModule() returns the module field; if null, calls like
  ;; getResourceAsStream() NPE.  An unnamed module (name=null) causes
  ;; isNamed() to return false, which skips module access checks.
  (classload "java/lang/Module")
  (let ((mod (%make-java-instance "java/lang/Module")))
    ;; Module(ClassLoader) constructor sets name=null, loader=classLoader
    (when (slot-exists-p mod '|name|)
      (setf (slot-value mod '|name|) nil))
    (when (slot-exists-p mod '|loader|)
      (setf (slot-value mod '|loader|) nil))
    (setf *unnamed-module* mod)
    ;; Set module on all existing Class objects
    (maphash (lambda (name klass)
               (declare (ignore name))
               (when (slot-exists-p klass '|module|)
                 (setf (slot-value klass '|module|) mod)))
             *java-classes-by-bin-name*))

  ;; ClassLoader.<init> may trigger ClassLoaders.<clinit> which calls
  ;; registerAsParallelCapable(). In JDK 17 this can throw InternalError
  ;; "Unable to register as parallel capable" — non-fatal for our purposes.
  (handler-case
      (|<init>()V| boot-class-loader)
    (condition (c)
      (format t "~&; Warning during ClassLoader init (non-fatal): ~A~%" c)))
  ;; JDK 17's ClassLoader() calls getSystemClassLoader() which returns
  ;; *boot-class-loader* — the same object! Clear parent to prevent
  ;; infinite recursion in getResources() delegation.
  (when (slot-exists-p boot-class-loader '|parent|)
    (setf (slot-value boot-class-loader '|parent|) nil)))

(defun %preload-lambda-interfaces (verb)
  "Load the functional interfaces backing native.lisp's Lambda* classes.
These must be fully defined (not forward-referenced) before anything
triggers finalize-inheritance on LambdaSupplier/LambdaPredicate/etc.
Use classload (not forName0): the defclass/std forms in native.lisp
create forward-referenced-class placeholders for these interfaces,
which makes forName0 believe they are already loaded and skip
generating the real CLOS class.  classload always emits it.  VERB
labels the non-fatal warning messages."
  (dolist (c '("java/util/function/Supplier"
               "java/util/function/Predicate"
               "java/util/function/BiPredicate"
               "java/util/function/Function"
               "java/util/function/Consumer"
               "java/util/function/BiConsumer"
               "java/util/function/BiFunction"
               "java/util/function/BinaryOperator"))
    (handler-case (classload c)
      (condition (e)
        (format t "~&; Warning ~A ~A (non-fatal): ~A~%" verb c e)))))

(defun %preload-runtime-classes (boot-class-loader)
  "Preload the ASM bytecode-generation classes used by LambdaMetafactory
and frequently loaded NIO/charset classes (testsuite performance).
MethodHandle/LambdaForm classes are deliberately NOT preloaded: their
lazy initialization via ClassSpecializer needs runtime ordering."
  ;; Minimal pre-load: only ASM bytecode generation classes
  ;; MethodHandle/LambdaForm classes have complex lazy initialization via ClassSpecializer
  ;; that doesn't work with pre-loading - they need runtime initialization in correct order
  (dolist (c '(;; ASM bytecode generation classes used by LambdaMetafactory
               "jdk/internal/org/objectweb/asm/ClassWriter"
               "jdk/internal/org/objectweb/asm/ClassVisitor"
               "jdk/internal/org/objectweb/asm/MethodVisitor"
               "jdk/internal/org/objectweb/asm/MethodWriter"
               "jdk/internal/org/objectweb/asm/FieldVisitor"
               "jdk/internal/org/objectweb/asm/FieldWriter"
               "jdk/internal/org/objectweb/asm/Type"
               "jdk/internal/org/objectweb/asm/Label"
               "jdk/internal/org/objectweb/asm/ByteVector"
               "jdk/internal/org/objectweb/asm/Item"
               "jdk/internal/org/objectweb/asm/Frame"
               "jdk/internal/org/objectweb/asm/Handler"
               "jdk/internal/org/objectweb/asm/Edge"
               "jdk/internal/org/objectweb/asm/AnnotationWriter"
               "jdk/internal/org/objectweb/asm/AnnotationVisitor"
               "jdk/internal/org/objectweb/asm/ClassReader"
               "jdk/internal/org/objectweb/asm/Handle"
               ;; Frequently loaded NIO/charset classes for testsuite performance
               "java/io/InterruptedIOException"
               "java/nio/BufferOverflowException"
               "java/nio/BufferUnderflowException"
               "java/nio/charset/CoderMalfunctionError"
               "java/nio/charset/CoderResult"
               "java/nio/charset/CoderResult$1"
               "java/nio/charset/CoderResult$2"
               "java/nio/charset/CoderResult$Cache"
               "java/nio/HeapCharBuffer"
               "java/nio/ReadOnlyBufferException"
               "java/lang/Readable"
               "java/nio/CharBuffer"
               "sun/nio/cs/Surrogate$Parser"))
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)Ljava/lang/Class;| (jstring c) nil boot-class-loader nil)))

(defun initialize (&optional (property-alist (list)))
  "Bootstrap the Java runtime: classpath, core classes, system
properties, System.initPhase1, and class preloading.  PROPERTY-ALIST
supplies additional system properties as (name . value) pairs."
  (assert (typep property-alist 'list))
  ;; The policy JIT-compiled code is built with, which the images save.
  ;; openldk.asd proclaims it around compiling OpenLDK itself, but an image
  ;; built from already-compiled files would otherwise keep the default.
  (proclaim '(optimize (speed 3) (safety 1) (debug 1)))
  (ensure-java-home)
  ;; Allow build-time control of DCE via environment (since main isn't invoked during image build)
  (let ((LDK_DCE (uiop:getenv "LDK_DCE")))
    (when (and LDK_DCE (plusp (length LDK_DCE)))
      (setf *enable-dce* t)))
  (%initialize-classpath)
  (%initialize-bootstrap-classes)
  (handler-case
      (let ((boot-class-loader (%make-java-instance "java/lang/ClassLoader")))
        (setf *boot-class-loader* boot-class-loader)
        (%initialize-primitive-classes)
        (%preload-early-classes boot-class-loader)
        (%seed-system-properties property-alist)
        (%initialize-system-phase1 boot-class-loader)
        (%preload-lambda-interfaces "preloading")
        (%preload-runtime-classes boot-class-loader))
    (|condition-java/lang/Throwable| (c)
      (let ((throwable (when (slot-boundp c '|objref|)
                         (slot-value c '|objref|))))
        (cond
          ((typep throwable '|java/lang/Throwable|)
           (format *error-output* "~&Unhandled Java exception:~%")
           (%print-java-stack-trace throwable :stream *error-output*)
           (finish-output *error-output*))
          (t
           (format *error-output* "~&Unhandled Java condition: ~A~%" c))))))
  ;; Re-ensure the functional interfaces are fully defined.  Done here
  ;; (outside the handler-case, after loader setup has finished) it
  ;; reliably emits the CLOS classes so the Lambda* subclasses can
  ;; finalize their inheritance at runtime.
  (%preload-lambda-interfaces "finalizing")
  (%adopt-native-short-methods)
  (%adopt-standard-java-gfs)
  (setf *debug-load* nil)
  (setf *debug-compile* nil))
