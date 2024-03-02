(in-package :openldk)

(defclass ssa-node ()
  ((address :reader .address :initarg :address :accessor address :initform -1)))

(defmethod print-object ((node ssa-node) out)
  (print-unreadable-object (node out :type t)
    (format out "~A" (slot-value node 'address))))

(defmethod dot-dump-string ((node ssa-node))
  (format nil "~3A: ~A" (.address node) (class-name (class-of node))))

(defmethod uses-stack-p ((node ssa-node))
  nil)

(defclass ssa-aaload (ssa-node)
  ())

(defclass ssa-aastore (ssa-node)
  ())

(defclass ssa-castore (ssa-node)
  ())

(defclass ssa-iastore (ssa-node)
  ())

(defclass ssa-aload (ssa-node)
  ((index :initarg :index)))

(defclass ssa-nop (ssa-node)
  ())

(defclass ssa-literal (ssa-node)
  ((value :initarg :value :initform nil)))

(defmethod emit ((v ssa-literal) cp)
  (slot-value v 'value))

(defclass ssa-local-variable (ssa-node)
  ((index :initarg :index)))

(defclass ssa-null-literal (ssa-literal)
  ())

(defclass ssa-string-literal (ssa-literal)
  ())

(defclass ssa-double-literal (ssa-literal)
  ())

(defclass ssa-int-literal (ssa-literal)
  ())

(defclass ssa-class (ssa-node)
  ((class :initarg :class)))

(defclass ssa-array-length (ssa-node)
  ())

(defclass ssa-assign (ssa-node)
  ((target :initarg :target)
   (source :initarg :source)))

(defclass ssa-add (ssa-node)
  ())

(defmethod uses-stack-p (ssa-add)
  t)

(defclass ssa-branch (ssa-node)
  ((offset :initarg :offset)
   (successors :initform nil)))

(defclass ssa-div (ssa-node)
  ())

(defclass ssa-dup (ssa-node)
  ())

(defclass ssa-dup-x1 (ssa-node)
  ())

(defclass ssa-goto (ssa-branch)
  ())

(defclass ssa-iaload (ssa-node)
  ())

(defclass ssa-iinc (ssa-node)
  ((index :initarg :index)
   (const :initarg :const)))

(defclass ssa-if-acmpeq (ssa-branch)
  ())

(defclass ssa-if-acmpne (ssa-branch)
  ())

(defclass ssa-if-icmpge (ssa-branch)
  ())

(defclass ssa-if-icmpgt (ssa-branch)
  ())

(defclass ssa-if-icmpeq (ssa-branch)
  ())

(defclass ssa-if-icmple (ssa-branch)
  ())

(defclass ssa-if-icmpne (ssa-branch)
  ())

(defclass ssa-ifeq (ssa-branch)
  ())

(defclass ssa-ifge (ssa-branch)
  ())

(defclass ssa-ifle (ssa-branch)
  ())

(defclass ssa-ifne (ssa-branch)
  ())

(defclass ssa-ifnonnull (ssa-branch)
  ())

(defclass ssa-ifnull (ssa-branch)
  ())

(defclass ssa-ineg (ssa-node)
  ())

(defclass ssa-instanceof (ssa-node)
  ((class :initarg :class)))

(defclass ssa-ishl (ssa-node)
  ())

(defclass ssa-ishr (ssa-node)
  ())

(defclass ssa-call (ssa-node)
  ())

(defclass ssa-branch-target (ssa-node)
  ((index :initarg :index)))

(defclass ssa-call-special-method (ssa-call)
  ((class :initarg :class)
   (method-name :initarg :method-name)
   (args :initarg :args :initform nil)))

(defclass ssa-call-virtual-method (ssa-call)
  ((method-name :initarg :method-name)
   (args :initarg :args :initform nil)))

(defclass ssa-call-static-method (ssa-call-virtual-method)
  ((class :initarg :class)))

(defclass ssa-checkcast (ssa-node)
  ((index :initarg :index)))

(defclass ssa-clinit (ssa-call)
  ((class :initarg :class)))

(defclass ssa-member (ssa-node)
  ((member-name :initarg :member-name)))

(defclass ssa-static-member (ssa-node)
  ((class :initarg :class)
   (member-name :initarg :member-name)))

(defclass ssa-store (ssa-node)
  ((target :initarg :target)))

(defclass ssa-if (ssa-node)
  ((condition :initarg :condition)
   (branch-if-true :initarg :branch-if-true)
   (branch-if-false :initarg :branch-if-false)))

(defclass ssa-new (ssa-node)
  ((class :initarg :class)))

(defclass ssa-new-array (ssa-new)
  ())

(defclass ssa-lushr (ssa-node)
  ())

(defclass ssa-mul (ssa-node)
  ())

(defclass ssa-pop (ssa-node)
  ())

(defclass ssa-push (ssa-node)
  ((value :initarg :value)))

(defmethod uses-stack-p ((node ssa-node))
  t)

(defclass ssa-return (ssa-node)
  ())

(defclass ssa-return-value (ssa-return)
  ((fn-name :initarg :fn-name)))

(defclass ssa-sub (ssa-node)
  ())

(defclass ssa-throw (ssa-branch)
  ())

(defclass ssa-variable (ssa-node)
  ((name :initarg :name)))
