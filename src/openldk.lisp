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

;; This is a hack to make sure getCallerClass works.
;; It would be good if we didn't have to do this.
(defvar *force-this-to-be-used* nil)

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
    ;; Then interrupt other threads to get their backtraces
    (dolist (thread all-threads)
      (unless (eq thread current)
        (format stream "~&~%=== Thread: ~A ===~%" thread)
        (ignore-errors
          (sb-thread:interrupt-thread
           thread
           (lambda ()
             (ignore-errors
               (sb-debug:print-backtrace :stream stream :print-thread nil)))))
        ;; Give the thread a moment to print
        (sleep 0.05))))
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
  (let ((fibers (sb-thread:list-all-fibers)))
    (when fibers
      (format stream "~&~%~80,,,'=@<~>~%")
      (format stream "Fiber dump (~D fiber~:P)~%" (length fibers))
      (format stream "~80,,,'=@<~>~%")
      (dolist (fiber fibers)
        (let* ((state (sb-thread:fiber-state fiber))
               (name (sb-thread:fiber-name fiber))
               (java-thread (gethash fiber *fiber-to-java-threads*)))
          (format stream "~&~%=== Fiber: ~A  state: ~A ===~%"
                  (or name fiber) state)
          (when java-thread
            (format stream "  Java thread: ~A~%" java-thread))
          (case state
            ((:suspended :created)
             (ignore-errors
               (let ((bt (sb-thread:fiber-get-backtrace fiber)))
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
             (string= (slot-value method 'descriptor) "(Lcom/sun/tools/javac/code/Type;)V"))
        ;; MethodHandles$Lookup security check bypassed to allow lambda metafactory
        (and (string= class-name "java/lang/invoke/MethodHandles$Lookup")
             (string= (slot-value method 'name) "checkUnprivilegedlookupClass"))
        ;; Generic type methods on Method - native stubs return non-generic types
        (and (string= class-name "java/lang/reflect/Method")
             (member (slot-value method 'name)
                     '("getGenericReturnType" "getGenericParameterTypes")
                     :test #'string=)))))

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
          (#+ansi-cl
           (style-warning (lambda (c)
                            (declare (ignore c))
                            (invoke-restart 'muffle-warning)))
)
        (eval code)))) ; lint:suppress eval-usage

(defun lispize-method-name (name)
  "Return a Lisp symbol name derived from Java method NAME."
  (take (1+ (position #\) name)) name))

(defun %compute-invoke-special-entry (gf owner-class nargs)
  "Compute the invoke-special cache entry for GF dispatching on OWNER-CLASS.
Returns a cons (method-fn . next-methods)."
  (let* ((class-list (cons owner-class
                           (make-list (max 0 (1- nargs))
                                      :initial-element (find-class 't))))
         (methods (closer-mop:compute-applicable-methods-using-classes gf class-list)))
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
          (setf entry (%compute-invoke-special-entry gf owner-class (length args)))
          (when entry
            (bordeaux-threads:with-lock-held ((java-gf-cache-lock gf))
              (setf (gethash owner-class cache) entry))))
        (when entry
          (return-from invoke-special
            (funcall (car entry) args (cdr entry))))))
    ;; Slow path: full MOP lookup for non-java-generic-function GFs
    (let* ((class-list (cons owner-class
                             (loop repeat (max 0 (1- (length args)))
                                   collect (find-class 't))))
           (methods (closer-mop:compute-applicable-methods-using-classes gf class-list)))
      (unless methods
        (error "No applicable methods found for ~A on declaring class ~A with ~D args"
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
                             (%class-loader-package class))))
      (if (static-p method)
          (setf (symbol-function fn-symbol) native-fn)
          (let* ((class-sym (intern (slot-value class 'name)
                                    (%class-specializer-package class)))
                 (n-params (count-parameters (slot-value method 'descriptor)))
                 (param-names (loop for i from 1 upto n-params
                                    collect (intern (format nil "arg~A" i) :openldk)))
                 (this-sym (intern "this" :openldk)))
            (%eval `(defmethod ,fn-symbol
                        ((,this-sym ,class-sym) ,@param-names)
                      (funcall ,native-fn ,this-sym ,@param-names))))))
    (setf (gethash method-key *methods-being-compiled*) :done)
    t))

(defun %claim-method-compilation (method-key)
  "Atomically claim METHOD-KEY for compilation by this thread.  Return T if
the caller should compile it, NIL if it is already compiled (or, in AOT mode,
being compiled elsewhere).  Blocks while another thread is compiling it."
  (bt:with-lock-held (*method-compilation-lock*)
    (loop
      (let ((status (gethash method-key *methods-being-compiled*)))
        (cond
          ((eq status :done)
           ;; Already compiled by another thread
           (return nil))
          ((eq status t)
           ;; In AOT mode, don't wait - just skip methods being compiled to
           ;; avoid recursion; in normal mode, wait for the other thread.
           (if *aot-dir*
               (return nil)
               (bt:condition-wait *method-compilation-cv* *method-compilation-lock*)))
          (t
           (setf (gethash method-key *methods-being-compiled*) t)
           (return t)))))))

(defun %transpile-method-bytecode (method-key code length)
  "Transpile METHOD-KEY's bytecode CODE into IR: run the per-opcode
transpilers, tracking reachability and per-PC stack state in *CONTEXT*;
then unify stack variables across control-flow joins and batch array
initializations.  Returns the IR instruction list."
  (let* ((exception-handler-table (make-exception-handler-table *context*))
         (branch-targets (%find-branch-targets code length))
         (in-dead-code nil) ;; Track unreachable code after unconditional branches
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
                                       (setf (stack *context*) (car stk))
                                       (setf in-dead-code nil)
                                       (setf (in-dead-code *context*) nil)
                                       (setf was-in-dead-code nil)))
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
                                         (let ((var (make-stack-variable *context* pc-start :REFERENCE)))
                                           (push var (stack *context*))
                                           (cons (make-instance 'ir-assign
                                                                :address pc-start
                                                                :lvalue var
                                                                :rvalue (make-instance 'ir-condition-exception))
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
                    when no-record-stack-state?
                      do (unless (gethash (pc *context*) branch-targets)
                           (setf in-dead-code t)
                           (setf (in-dead-code *context*) t))
                    unless (or was-in-dead-code no-record-stack-state?)
                      do (%record-stack-state (pc *context*) *context*)
                    unless (or (null result) was-in-dead-code)
                      collect result))))
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

(defun %build-method-definition (class method blocks-after-dce parameter-hints max-locals)
  "Build the complete DEFUN/DEFMETHOD form for METHOD from its optimized
BLOCKS-AFTER-DCE: generate each block's code, then wrap it with debug
tracing, the NPE/array-bounds handler-binds, and ACC_SYNCHRONIZED monitor
entry/exit as required."
  (let* ((lisp-code
           (list (list 'block nil
                       (append (list 'tagbody)
                               (mapcan (lambda (x) (if (listp x) x (list x)))
                                       (loop for block in blocks-after-dce
                                             for code = (codegen-block block block)
                                             when code
                                               collect code))))))
         (traced-lisp-code (if *debug-trace* `((unwind-protect
                                                    ,(car lisp-code)
                                                 (incf *call-nesting-level* -1)))
                               lisp-code))
         ;; Always install memory-fault-error and type-error handlers
         ;; to convert null pointer dereferences (SIGSEGV at NIL+offset)
         ;; to Java NullPointerException.  Array bounds handler is only
         ;; needed when the method contains array operations.
         (null-checked-lisp-code
           `((handler-bind
                 (,@(when (needs-array-bounds-check *context*)
                      `((sb-int:invalid-array-index-error
                          (lambda (e)
                            (declare (ignore e))
                            (error (openldk::%lisp-condition
                                    (openldk::%make-throwable
                                     'openldk::|java/lang/ArrayIndexOutOfBoundsException|)))))))
                  (sb-sys:memory-fault-error
                    (lambda (e)
                      (declare (ignore e))
                      (error (openldk::%lisp-condition
                              (openldk::%make-throwable
                               'openldk::|java/lang/NullPointerException|)))))
                  (type-error
                    (lambda (e)
                      (declare (ignore e))
                      (error (openldk::%lisp-condition
                              (openldk::%make-throwable
                               'openldk::|java/lang/NullPointerException|))))))
               ,(car traced-lisp-code))))
         ;; For ACC_SYNCHRONIZED methods, wrap body with monitor-enter/exit.
         ;; Instance methods synchronize on 'this'; static methods on the Class object.
         (synchronized-lisp-code
           (if (synchronized-p method)
               (let ((monitor-obj
                       (if (static-p method)
                           `(openldk::|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)|
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
        `(progn
           ,(append (if (static-p method)
                        (list 'defun (intern (fn-name *context*) class-pkg) args)
                        (list 'defmethod
                              (intern (fn-name *context*) :openldk)
                              (cons (list (intern "this" :openldk) (intern (slot-value class 'name) class-pkg))
                                    args)))
                    (when *debug-trace*
                      (list (list 'format 't "~&~V@A <~A> trace: entering ~A.~A(~{~A~^ ~}) ~A~%"
                                  (list 'incf '*call-nesting-level* 1) "*" '*call-nesting-level*
                                  (slot-value class 'name) (fn-name *context*)
                                  (if *debug-trace-args*
                                      (cons 'list args)
                                      ())
                                  (if (not (static-p method)) (intern "this" :openldk) ""))))
                    (when (not (static-p method))
                      (list (list 'setf '*force-this-to-be-used* (intern "this" :openldk))))
                    (let ((i 0)
                          (pc -1))
                      (list (format nil "bridge=~A" (bridge-p method))
                            (append (list 'let (if (static-p method)
                                                   (append
                                                    (list (list '|condition-cache|))
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
                                                    (list (list '|condition-cache|))
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
                                                           collect (list (intern (format nil "local-~A" (incf i)) :openldk)))))))
                                    synchronized-lisp-code)))))))))

(defun %install-method-definition (class method method-key definition-code length)
  "Evaluate DEFINITION-CODE to install METHOD's compiled function, working
around SBCL native-compiler limits on very large method bodies: interpret
huge bytecode outright, and fall back to interpretation when native
compilation fails silently (leaving the self-compiling stub in place)."
  (handler-case
      ;; Very large bytecode (e.g. 1000+ element array initializers)
      ;; can cause SBCL's native compiler to hang or abort.  Use the
      ;; interpreter directly for those methods.
      (if (> length 5000)
          (let ((sb-ext:*evaluator-mode* :interpret))
            (eval definition-code))
          (%eval definition-code))
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
          ;; Check if the method's source-name still references
          ;; %compile-method — i.e. the stub was never replaced.
          (when (or (null our-method)
                    (let ((mf (sb-mop:method-function our-method)))
                      (and mf
                           (search "%COMPILE-METHOD"
                                   (princ-to-string
                                    (sb-kernel:%fun-name mf))))))
            (format *error-output*
                    "~&;; COMPILE-FALLBACK: native compilation failed silently for ~A, retrying interpreted~%"
                    method-key)
            (force-output *error-output*)
            (handler-case
                (let ((sb-ext:*evaluator-mode* :interpret))
                  (eval definition-code)) ; lint:suppress eval-usage
              (error (c)
                (format *error-output*
                        "~&;; COMPILE-FALLBACK-ERROR in ~A: ~A~%"
                        method-key c)
                (force-output *error-output*)
                (error c)))))))))

(defun %install-jit-error-stub (class method method-key)
  "Install a function that signals on invocation, so a failed JIT compile
doesn't leave a self-recursing compilation stub behind.  Uses plain Lisp
errors (not Java exceptions) to avoid triggering further class loading
during error handling."
  (let ((fn-symbol (intern (%method-fn-name class method)
                           (%class-loader-package class))))
    (if (static-p method)
        (when (fboundp fn-symbol)
          (setf (symbol-function fn-symbol)
                (lambda (&rest args)
                  (declare (ignore args))
                  (error "JIT compilation failed for ~A" method-key))))
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
                     (error "JIT compilation failed for ~A" ,method-key))))))))

(defun %compile-method (class-name method-index)
  "JIT-compile method METHOD-INDEX of CLASS-NAME: transpile its bytecode to
IR, optimize, generate Lisp code, and install the resulting function
(or write it out in AOT mode).  Thread-safe; concurrent callers wait for
or skip in-progress compilations."
  (let* ((class (%get-ldk-class-by-bin-name class-name))
         (method (aref (slot-value class 'methods) (1- method-index)))
         (method-key (format nil "~A.~A~A" class-name (slot-value method 'name) (slot-value method 'descriptor))))
    ;; Skip bytecode compilation for methods with native overrides.
    ;; Replace the stub with the native function so the stub doesn't loop.
    (when (%install-native-override class method method-key)
      (return-from %compile-method nil))
    (unless (%claim-method-compilation method-key)
      (return-from %compile-method nil))
    (let ((compilation-completed nil))
      (unwind-protect
           (handler-case
               (progn
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
                                                    :is-clinit-p (string= "<clinit>" (slot-value method 'name)))))
                     (setf (svcount *context*) 0)
                     (when *debug-bytecode*
                       (format t "~&; COMPILING ~A~%" method-key))
                     (setf (fn-name *context*) (%method-fn-name class method))
                     (let* ((ir-code (setf (ir-code *context*)
                                           (%transpile-method-bytecode method-key code length)))
                            (blocks-after-dce (%optimize-method-blocks ir-code))
                            (definition-code (%build-method-definition class method blocks-after-dce
                                                                       parameter-hints max-locals)))
                       (when (search "require" method-key)
                         (format *error-output* "~&;; COMPILING METHOD: ~A~%" method-key)
                         (force-output *error-output*))
                       (if *aot-dir*
                           (%write-aot-method class-name
                                              (lispize-method-name (format nil "~A~A" (name method) (descriptor method)))
                                              definition-code)
                           (%install-method-definition class method method-key definition-code length))
                       (when (or *debug-compile* *debug-codegen*)
                         (format t "; COMPILING ~A.~A (~Dms)~%"
                                 class-name
                                 (lispize-method-name (format nil "~A~A" (name method) (descriptor method)))
                                 (round (* 1000 (/ (- (get-internal-real-time) compile-start-time)
                                                   internal-time-units-per-second))))
                         (force-output)))))
                 (setf compilation-completed t))
             (error (c)
               ;; Compilation failed (IR translation, codegen, or eval error).
               (format *error-output* "~&;; JIT-ERROR in ~A: ~A~%" method-key c)
               (when *debug-codegen*
                 (sb-debug:print-backtrace :stream *error-output* :count 60))
               (force-output *error-output*)
               (%install-jit-error-stub class method method-key)
               (setf compilation-completed t)))
        ;; Cleanup: mark compilation as done and notify waiting threads.
        ;; If compilation was interrupted (e.g. by timeout or stack overflow),
        ;; clear the status so a future call can retry.
        (bt:with-lock-held (*method-compilation-lock*)
          (if compilation-completed
              (setf (gethash method-key *methods-being-compiled*) :done)
              (remhash method-key *methods-being-compiled*))
          (bt:condition-notify *method-compilation-cv*))))))


(defun %clinit (class)
  (let ((class (gethash (name class) *ldk-classes-by-bin-name*)))
    (assert
     (or class (error "Can't find ~A" class)))
    (labels ((clinit (class)
               (let ((super-class (gethash (slot-value class 'super) *ldk-classes-by-bin-name*)))
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
                     (when (not (initialized-p class))
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
                         (%eval (list (intern (format nil "~A.<clinit>()" (slot-value class 'name)) pkg)))
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
                         ;; Wrap exception in ExceptionInInitializerError if classes are loaded
                         ;; Use ignore-errors to handle case where parent class isn't loaded yet
                         (let ((eiie (ignore-errors
                                       (when (and (find-class '|java/lang/ExceptionInInitializerError| nil)
                                                  (find-class '|java/lang/Error| nil))
                                         (let* ((instance (%make-java-instance "java/lang/ExceptionInInitializerError"))
                                                (cause (and (typep throwable '|java/lang/Throwable|) throwable)))
                                           (if cause
                                               (|<init>(Ljava/lang/Throwable;)| instance cause)
                                               (|<init>()| instance))
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

(defun %topo-sort-interfaces (ifaces)
  "Sort interface symbols so that subtypes come before supertypes.
   For CLOS C3 linearization, if A is a subtype of B, A must come before B.
   First removes redundant interfaces to avoid CPL circularity.
   Uses an insertion-based approach that compares all pairs."
  (when (null ifaces)
    (return-from %topo-sort-interfaces nil))
  (setf ifaces (%remove-redundant-interfaces ifaces))
  (let ((result (copy-list ifaces)))
    ;; For each pair (i, j) where i < j, if result[j] is a supertype of result[i],
    ;; that's fine. But if result[i] is a supertype of result[j], we need to move
    ;; result[j] before result[i].
    ;; Repeat until stable.
    (loop with changed = t
          while changed
          do (setf changed nil)
             (loop for i from 0 below (1- (length result))
                   do (loop for j from (1+ i) below (length result)
                            do (let ((a (nth i result))
                                     (b (nth j result)))
                                 (let ((fc-a (find-class a nil))
                                       (fc-b (find-class b nil)))
                                   ;; If B is a (strict) subtype of A, B should come before A
                                   (when (and fc-a fc-b
                                              (subtypep fc-b fc-a)
                                              (not (subtypep fc-a fc-b)))
                                     ;; Move B to position i (before A)
                                     (setf result (append (subseq result 0 i)
                                                          (list b)
                                                          (subseq result i j)
                                                          (subseq result (1+ j))))
                                     (setf changed t)
                                     (return)))))))
    result))

(defun %field-shadows-super-p (class field ldk-loader)
  "True when instance FIELD of CLASS shadows an instance field of the same
name declared somewhere in CLASS's superclass chain (distinct fields in
the JVM -- javac's synthetic this$0 outer references do this routinely)."
  (and (zerop (logand 8 (slot-value field 'access-flags)))   ; instance field
       (let ((field-name (slot-value field 'name)))
         (loop with cname = (slot-value class 'super)
               while cname
               for super-class = (find-class-in-loader-hierarchy cname ldk-loader)
               while super-class
               when (find-if (lambda (sf)
                               (and (string= (slot-value sf 'name) field-name)
                                    (zerop (logand 8 (slot-value sf 'access-flags)))))
                             (fields super-class))
                 return t
               do (setf cname (slot-value super-class 'super))))))

(defun %field-slot-symbol (class field ldk-loader)
  "CLOS slot symbol for FIELD of CLASS: the plain mangled name, or a
class-qualified symbol (recorded in *FIELD-SHADOW-SLOTS*) when the field
shadows a superclass field and needs its own storage."
  (let ((mangled (mangle-field-name (slot-value field 'name))))
    (if (%field-shadows-super-p class field ldk-loader)
        (let ((sym (intern (format nil "~A$~A" mangled (slot-value class 'name)) :openldk)))
          (setf (gethash (format nil "~A.~A" (slot-value class 'name) (slot-value field 'name))
                         *field-shadow-slots*)
                sym)
          sym)
        (intern mangled :openldk))))

(defun emit-<class> (class ldk-loader)
  "Emit CLOS class and method definitions for a Java class.
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
               (map 'list
                    (lambda (f)
                      (list (%field-slot-symbol class f ldk-loader)
                            :initform (let ((cf (gethash "ConstantValue" (slot-value f 'attributes))))
                                        (if cf
                                            (value (emit (aref (constant-pool class) cf) (constant-pool class)))
                                            (initform-from-descriptor (slot-value f 'descriptor))))
                            :allocation
                            (if (eq 0 (logand 8 (slot-value f 'access-flags))) :instance :class)))
                    fields))
              (list
               'defparameter (intern (format nil "+static-~A+" (intern name pkg)) pkg)
               (list
                'make-instance (list 'quote (intern name pkg)))))))
        (methods-code
          (let ((method-index 0)
                (done-method-table (make-hash-table :test #'equal)))
            (if (find (name class) '("java/util/jar/JarInputStream" "java/util/zip/ZipFile" "java/util/zip/ZipInputStream") :test #'equal)
                nil
                (with-slots (name super methods) class
                  (remove nil (map 'list
                                   (lambda (m)
                                     (if (or (native-p m)
                                             (native-override-p m)
                                             (null (gethash "Code" (attributes m)))
                                             (and (bridge-p m)
                                                  (gethash (lispize-method-name
                                                            (format nil "~A~A"
                                                                    (slot-value m 'name)
                                                                    (slot-value m 'descriptor)))
                                                           done-method-table)))
                                         (progn
                                           (incf method-index)
                                           nil)
                                         (progn
                                           (setf (gethash (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) done-method-table) t)
                                           (if (static-p m)
                                               ;; Static methods: function name in loader's package (includes class name)
                                               (list 'defun
                                                     (intern (format nil "~A.~A"
                                                                     (slot-value class 'name)
                                                                     (lispize-method-name
                                                                      (format nil "~A~A"
                                                                              (slot-value m 'name)
                                                                              (slot-value m 'descriptor))))
                                                             pkg)
                                                     (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                           collect (intern (format nil "arg~A" i) :openldk))
                                                     (list '%compile-method (slot-value class 'name) (incf method-index))
                                                     (cons (intern (format nil "~A.~A"
                                                                           (slot-value class 'name)
                                                                           (lispize-method-name
                                                                            (format nil "~A~A"
                                                                                    (slot-value m 'name)
                                                                                    (slot-value m 'descriptor))))
                                                                   pkg)
                                                           (loop for i from 1 upto (count-parameters (slot-value m 'descriptor))
                                                                 collect (intern (format nil "arg~A" i) :openldk))))
                                               ;; Instance methods: generic function name in :openldk for dispatch,
                                               ;; class specializer in loader's package for isolation.
                                               ;; Pre-create GF with java-generic-function metaclass for fast dispatch.
                                               (let ((method-name (intern (lispize-method-name (format nil "~A~A" (slot-value m 'name) (slot-value m 'descriptor))) :openldk))
                                                     (param-count (count-parameters (slot-value m 'descriptor))))
                                                 (list 'progn
                                                       (list 'unless
                                                             (list 'and
                                                                   (list 'fboundp (list 'quote method-name))
                                                                   (list 'typep (list 'symbol-function (list 'quote method-name)) ''generic-function))
                                                             (list 'ensure-generic-function (list 'quote method-name)
                                                                   :generic-function-class ''java-generic-function
                                                                   :lambda-list (list 'quote
                                                                                      (cons (intern "this" :openldk)
                                                                                            (loop for i from 1 upto param-count
                                                                                                  collect (intern (format nil "arg~A" i) :openldk))))))
                                                       (list 'defmethod method-name
                                                             (cons (list (intern "this" :openldk) (intern (slot-value (slot-value m 'class) 'name) pkg))
                                                                   (loop for i from 1 upto param-count
                                                                         collect (intern (format nil "arg~A" i) :openldk)))
                                                             (list '%compile-method (slot-value class 'name) (incf method-index))
                                                             (list 'invoke-special
                                                                   (list 'quote method-name)
                                                                   (list 'quote (intern (slot-value (slot-value m 'class) 'name) pkg))
                                                                   (cons 'list
                                                                         (cons (intern "this" :openldk)
                                                                               (loop for i from 1 upto param-count
                                                                                     collect (intern (format nil "arg~A" i) :openldk))))))))))))
                                   methods)))))))

    (append defclass-code methods-code)))


(defun find-class-in-loader-hierarchy (classname ldk-loader)
  "Search for class in this loader and all parent loaders sharing the same package."
  (when ldk-loader
    (or (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))
        (let ((parent (slot-value ldk-loader 'parent-loader)))
          (when parent
            (find-class-in-loader-hierarchy classname parent))))))

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
  (unwind-protect
       (let* ((pkg (loader-package ldk-loader))
              (classname-symbol (intern classname pkg))
              (fq-classname (cl-ppcre:regex-replace-all "\\.anonymous-class"
                                                        (substitute #\. #\/ classname)
                                                        "/anonymous-class"))
              (class
                (let ((c (read-classfile classfile-stream)))
                  (setf (name c) classname)
                  (setf (slot-value c 'ldk-loader) ldk-loader)
                  ;; Store in loader's hash tables
                  (setf (gethash classname (slot-value ldk-loader 'ldk-classes-by-bin-name)) c)
                  (setf (gethash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name)) c)
                  ;; Also store in global tables for backward compatibility
                  ;; This allows lookups without a specific loader to find dynamically loaded classes
                  (setf (gethash classname *ldk-classes-by-bin-name*) c)
                  (setf (gethash fq-classname *ldk-classes-by-fq-name*) c)
                  c))
              (super (let ((super (slot-value class 'super)))
                       (when super (classload super))))
              (interfaces (let ((interfaces (slot-value class 'interfaces)))
                            (when interfaces
                              (mapcar (lambda (i) (classload i)) (coerce interfaces 'list))))))
         ;; If superclass couldn't be loaded, bail out (like JVM NoClassDefFoundError).
         ;; Clean up the partially-registered class entries and return nil.
         (when (and (slot-value class 'super) (null super))
           (remhash classname (slot-value ldk-loader 'ldk-classes-by-bin-name))
           (remhash fq-classname (slot-value ldk-loader 'ldk-classes-by-fq-name))
           (remhash classname *ldk-classes-by-bin-name*)
           (remhash fq-classname *ldk-classes-by-fq-name*)
           (return-from %classload-from-stream nil))
         (let ((klass (or (%get-java-class-by-bin-name classname t ldk-loader)
                          (let ((klass (%make-java-instance "java/lang/Class"))
                                (cname (jstring fq-classname)))
                            (with-slots (|name| |classLoader|) klass
                              (setf |name| cname)
                              (setf |classLoader| class-loader))
                            ;; JDK 9+: set unnamed module so Class.getModule() is non-null
                            (when (and *unnamed-module* (slot-exists-p klass '|module|))
                              (setf (slot-value klass '|module|) *unnamed-module*))
                            klass))))
           (setf (java-class class) klass)
           ;; JDK 25: Class.getModifiers(), isInterface() and isEnum() read the
           ;; `modifiers` field directly (these are no longer native methods), so
           ;; populate it from the class file's access flags.
           (when (slot-exists-p klass '|modifiers|)
             ;; Class.getModifiers() reports source-level modifiers, not raw class
             ;; access flags: mask out ACC_SUPER (0x20), set on every modern class,
             ;; which would otherwise render as "synchronized".
             (setf (slot-value klass '|modifiers|) (logandc2 (access-flags class) #x20)))
           (setf (slot-value klass '|classLoader|) class-loader)
           ;; Store in loader's java-class hash tables
           (setf (gethash classname (slot-value ldk-loader 'java-classes-by-bin-name)) klass)
           (setf (gethash fq-classname (slot-value ldk-loader 'java-classes-by-fq-name)) klass)
           ;; Also store in global tables for backward compatibility
           (setf (gethash classname *java-classes-by-bin-name*) klass)
           (setf (gethash fq-classname *java-classes-by-fq-name*) klass))

         (let ((code (emit-<class> class ldk-loader)))
           ;; In AOT mode, extract and write class definitions separately
           (when *aot-dir*
             ;; Code is a list like (progn (defclass...) (defparameter...) method-stub1 method-stub2 ...)
             ;; Extract just the defclass and defparameter (elements 2 and 3)
             (let ((class-defs (if (and (listp code) (eq (first code) 'progn))
                                   ;; If it's a progn, take the defclass and defparameter (2nd and 3rd elements)
                                   (list 'progn (second code) (third code))
                                   ;; Otherwise, just take the first two elements
                                   (list 'progn (first code) (second code)))))
               (%write-aot-class classname class-defs)))
           (%eval code))

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
                                    for k-ldk-class = (%get-ldk-class-by-bin-name (format nil "~A" (class-name k)) t ldk-loader)
                                    ;; Each superclass's clinit is in its defining loader's package
                                    for k-pkg = (if k-ldk-class
                                                    (loader-package (slot-value k-ldk-class 'ldk-loader))
                                                    pkg)
                                    for clinit-function = (intern (format nil "~a.<clinit>()" (class-name k)) k-pkg)
                                    when (and k-ldk-class (fboundp clinit-function))
                                      collect (list 'unless (list 'initialized-p k-ldk-class)
                                                    (list 'setf (list 'initialized-p k-ldk-class) t)
                                                    (list clinit-function))))))
             (%eval icc)))

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

         class)
    (close classfile-stream)))

(defun classload (classname &optional (class-loader nil))
  "Load a class from the classpath using proper JVM class loader delegation.
   JDK classes (java/*, javax/*, sun/*, etc.) are loaded by boot loader.
   User classes are loaded by app loader (which delegates to boot loader first).
   CLASS-LOADER is the java.lang.ClassLoader object (usually NIL for system loader)."
  (let ((classname (coerce classname 'string)))
    (assert (not (find #\. classname)))
    (assert (> (length classname) 0))
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
      ;; Check if already loaded by boot loader
      ((gethash classname (slot-value *boot-ldk-class-loader* 'ldk-classes-by-bin-name)))
      ;; Check global tables (classes loaded during warm-up before loaders were created)
      ((gethash classname *ldk-classes-by-bin-name*))
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

(defun ensure-JAVA_HOME ()
  (let ((JAVA_HOME (uiop:getenv "JAVA_HOME")))
    (unless JAVA_HOME
      (format *error-output* "~%OpenLDK Error: JAVA_HOME environment variable not set~%")
      (format *error-output* "  Set JAVA_HOME to a JDK 25 installation (e.g. /usr/lib/jvm/java-25-openjdk).~%")
      (uiop:quit 1))

    (let ((major (%java-home-major-version JAVA_HOME)))
      (when (and major (/= major 25))
        (format *error-output* "~%OpenLDK Error: JAVA_HOME points at a JDK ~A installation:~%  ~A~%" major JAVA_HOME)
        (format *error-output* "  OpenLDK requires JDK 25. Set JAVA_HOME to a JDK 25 installation.~%")
        (uiop:quit 1)))

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
    (setf *vm-start-time-millis* (|java/lang/System.currentTimeMillis()|)))
  (ensure-JAVA_HOME)
  ;; Install SIGQUIT handler for debugging hangs (send kill -3 to dump all stacks)
  (install-sigquit-handler)
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
  ;; Application class loader: OPENLDK.APP package, child of boot.
  (unless *app-ldk-class-loader*
    (setf *app-ldk-class-loader*
          (make-ldk-class-loader :parent-loader *boot-ldk-class-loader*
                                 :java-loader nil
                                 :package-name "OPENLDK.APP")))
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
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)|
     (ijstring (car prop)) (ijstring (cdr prop))))
  (when *debug-slynk*
    (slynk:create-server :port 2025)
    (sleep 10))
  (setf *dump-dir* dump-dir)
  (setf *aot-dir* aot)
  (let ((classpath (or classpath
                       (uiop:getenv "CLASSPATH")
                       (uiop:getenv "LDK_CLASSPATH")
                       ".")))
    (setf *classpath*
          (append
           (loop for cpe in (split-sequence:split-sequence (uiop:inter-directory-separator) classpath)
                 collect (if (ends-with? ".jar" cpe)
                             (make-instance 'jar-classpath-entry :jarfile cpe)
                             (make-instance 'dir-classpath-entry :dir cpe)))
           (discover-jmod-classpath-entries)))))

(defun %find-java-main-symbol (class)
  "Find the static main([Ljava/lang/String;) symbol for CLASS, searching
the superclass chain.  Static methods live in the defining class's
loader package (OPENLDK.SYSTEM for classes loaded during warm-up)."
  (when class
    (let* ((pkg (if (and (slot-boundp class 'ldk-loader)
                         (slot-value class 'ldk-loader))
                    (loader-package (slot-value class 'ldk-loader))
                    (or (find-package "OPENLDK.SYSTEM")
                        (make-package "OPENLDK.SYSTEM" :use '(:openldk)))))
           (main-symbol (intern (format nil "~A.main([Ljava/lang/String;)" (name class)) pkg)))
      (if (fboundp main-symbol)
          main-symbol
          (%find-java-main-symbol (gethash (super class) *ldk-classes-by-bin-name*))))))

(defun %await-nondaemon-java-threads ()
  "Wait for all non-daemon Java threads (other than the current one) to
complete.  Interactive applications can disable the default safety
timeout with LDK_THREAD_WAIT_SECONDS=0."
  (let ((current-lisp-thread (bordeaux-threads:current-thread))
        (deadline
          (let* ((setting (uiop:getenv "LDK_THREAD_WAIT_SECONDS"))
                 (seconds (if setting
                              (parse-integer setting :junk-allowed t)
                              30)))
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
                                (and fiber (sb-thread:fiber-alive-p fiber))))
                      collect java-thread))
            #-sb-fiber
            (fiber-threads nil))
        (let ((java-threads (append platform-threads fiber-threads)))
          (cond
            ((null java-threads)
             (sleep 0.1)
             (finish-output)
             (return))
            ((and deadline (> (get-internal-real-time) deadline))
             (finish-output)
             (return))
            (t
             (sleep 0.1))))))))

(defun main (mainclass &optional (args (list)) &key dump-dir classpath aot)
  "Run a Java class with the given arguments.
   MAINCLASS: The class with the static main method to execute.
   ARGS: Java program command line arguments (list of strings).
   CLASSPATH: The classpath from which classes are loaded.
   DUMP-DIR: The directory into which internal debug info is dumped.
   AOT: Ahead-of-time compilation directory (generate Lisp source files)."
  (%main-runtime-setup classpath dump-dir aot)
  (if *aot-dir*
      (%run-aot-main mainclass aot)
      (let* ((class (classload (substitute #\/ #\. mainclass)))
             (argv (make-java-array
                    :component-class (%get-java-class-by-bin-name "java/lang/String")
                    :initial-contents (mapcar #'jstring args))))
        (assert (or class (error "Can't load ~A" mainclass)))
        (%clinit class)
        (let ((main-symbol (%find-java-main-symbol class)))
          (unless main-symbol
            (error "Main method not found in class ~A." (name class)))
          (%eval (list main-symbol argv))
          (%await-nondaemon-java-threads)))))


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
             (header (%java-string (|toString()| throwable))))
        (if prefix
            (format stream "~&~A~A~A~%" indent-str prefix header)
            (format stream "~&~A~A~%" indent-str header))
        (when (and (slot-boundp throwable '|backtrace|)
                   (slot-value throwable '|backtrace|))
          (let ((depth (|getStackTraceDepth()| throwable)))
            (dotimes (i depth)
              (let* ((ste (|getStackTraceElement(I)| throwable i))
                     (line (%java-string (|toString()| ste))))
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
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil boot-class-loader nil)))

(defun %seed-system-properties (property-alist)
  "Create the System props table and seed essential properties needed
during early JDK init (many JDK components assume non-NIL encodings),
plus user.* properties and the caller-supplied PROPERTY-ALIST."
  (let ((props (%make-java-instance "java/util/Properties")))
    (|<init>()| props)
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
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring (car kv)) (ijstring (cdr kv))))

  ;; Also set java.home for code that queries it early.
  (when-let ((jh (uiop:getenv "JAVA_HOME")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "java.home") (ijstring jh)))

  ;; Populate common user properties
  (when-let ((cwd (uiop:getcwd)))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "user.dir") (ijstring (namestring cwd))))
  (when-let ((uh (or (uiop:getenv "HOME") "/")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "user.home") (ijstring uh)))
  (when-let ((un (or (uiop:getenv "USER") (uiop:getenv "LOGNAME") "openldk")))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring "user.name") (ijstring un)))

  ;; Add user-provided properties...
  (dolist (prop property-alist)
    (assert (typep prop 'list))
    (|java/lang/System.setProperty(Ljava/lang/String;Ljava/lang/String;)| (ijstring (car prop)) (ijstring (cdr prop)))))

(defun %initialize-system-phase1 (boot-class-loader)
  "Run System.initPhase1 and the module/class-loader setup around it."
  ;; Ensure AccessibleObject <clinit> runs before initPhase1,
  ;; because ReflectionFactory reads langReflectAccess from SharedSecrets
  ;; in its constructor, and AccessibleObject <clinit> is what sets it.
  (%clinit (classload "java/lang/reflect/AccessibleObject"))

  ;; Call System.initPhase1() which sets up JavaLangAccess (JLA),
  ;; system properties (via SystemProps$Raw native methods), I/O streams
  ;; (System.in/out/err), and VM.initLevel(1).
  (|java/lang/System.initPhase1()|)

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
      (|<init>()| boot-class-loader)
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
    (|java/lang/Class.forName0(Ljava/lang/String;ZLjava/lang/ClassLoader;Ljava/lang/Class;)| (jstring c) nil boot-class-loader nil)))

(defun initialize (&optional (property-alist (list)))
  "Bootstrap the Java runtime: classpath, core classes, system
properties, System.initPhase1, and class preloading.  PROPERTY-ALIST
supplies additional system properties as (name . value) pairs."
  (assert (typep property-alist 'list))
  (ensure-JAVA_HOME)
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
  (setf *debug-load* nil)
  (setf *debug-compile* nil))
