(in-package :openldk)

(defclass ir-node ()
  ((address :reader .address :initarg :address :accessor address :initform -1)))

(defmethod print-object ((node ir-node) out)
  (print-unreadable-object (node out :type t)
    (format out "~A" (slot-value node 'address))))

(defmethod dot-dump-string ((node ir-node))
  (format nil "~3A: ~A" (.address node) (class-name (class-of node))))

(defmethod uses-stack-p ((node ir-node))
  nil)

(defclass ir-aaload (ir-node)
  ())

(defclass ir-aastore (ir-node)
  ())

(defclass ir-castore (ir-node)
  ())

(defclass ir-iastore (ir-node)
  ())

(defclass ir-aload (ir-node)
  ((index :initarg :index)))

(defclass ir-nop (ir-node)
  ())

(defclass ir-literal (ir-node)
  ((value :initarg :value :initform nil)))

(defmethod emit ((v ir-literal) cp)
  (slot-value v 'value))

(defclass ir-local-variable (ir-node)
  ((index :initarg :index)))

(defclass ir-null-literal (ir-literal)
  ())

(defclass ir-string-literal (ir-literal)
  ())

(defclass ir-double-literal (ir-literal)
  ())

(defclass ir-int-literal (ir-literal)
  ())

(defclass ir-class (ir-node)
  ((class :initarg :class)))

(defclass ir-array-length (ir-node)
  ())

(defclass ir-assign (ir-node)
  ((target :initarg :target)
   (source :initarg :source)))

(defclass ir-add (ir-node)
  ())

(defmethod uses-stack-p (ir-add)
  t)

(defclass ir-branch (ir-node)
  ((offset :initarg :offset)
   (successors :initform nil)))

(defclass ir-div (ir-node)
  ())

(defclass ir-dup (ir-node)
  ())

(defclass ir-dup-x1 (ir-node)
  ())

(defclass ir-goto (ir-branch)
  ())

(defclass ir-iaload (ir-node)
  ())

(defclass ir-iinc (ir-node)
  ((index :initarg :index)
   (const :initarg :const)))

(defclass ir-if-acmpeq (ir-branch)
  ())

(defclass ir-if-acmpne (ir-branch)
  ())

(defclass ir-if-icmpge (ir-branch)
  ())

(defclass ir-if-icmpgt (ir-branch)
  ())

(defclass ir-if-icmpeq (ir-branch)
  ())

(defclass ir-if-icmple (ir-branch)
  ())

(defclass ir-if-icmpne (ir-branch)
  ())

(defclass ir-ifeq (ir-branch)
  ())

(defclass ir-ifge (ir-branch)
  ())

(defclass ir-ifle (ir-branch)
  ())

(defclass ir-ifne (ir-branch)
  ())

(defclass ir-ifnonnull (ir-branch)
  ())

(defclass ir-ifnull (ir-branch)
  ())

(defclass ir-ineg (ir-node)
  ())

(defclass ir-instanceof (ir-node)
  ((class :initarg :class)))

(defclass ir-ishl (ir-node)
  ())

(defclass ir-ishr (ir-node)
  ())

(defclass ir-call (ir-node)
  ())

(defclass ir-branch-target (ir-node)
  ((index :initarg :index)))

(defclass ir-call-special-method (ir-call)
  ((class :initarg :class)
   (method-name :initarg :method-name)
   (args :initarg :args :initform nil)))

(defclass ir-call-virtual-method (ir-call)
  ((method-name :initarg :method-name)
   (args :initarg :args :initform nil)))

(defclass ir-call-static-method (ir-call-virtual-method)
  ((class :initarg :class)))

(defclass ir-checkcast (ir-node)
  ((index :initarg :index)))

(defclass ir-clinit (ir-call)
  ((class :initarg :class)))

(defclass ir-member (ir-node)
  ((member-name :initarg :member-name)))

(defclass ir-static-member (ir-node)
  ((class :initarg :class)
   (member-name :initarg :member-name)))

(defclass ir-store (ir-node)
  ((target :initarg :target)))

(defclass ir-if (ir-node)
  ((condition :initarg :condition)
   (branch-if-true :initarg :branch-if-true)
   (branch-if-false :initarg :branch-if-false)))

(defclass ir-new (ir-node)
  ((class :initarg :class)))

(defclass ir-new-array (ir-new)
  ())

(defclass ir-lushr (ir-node)
  ())

(defclass ir-mul (ir-node)
  ())

(defclass ir-pop (ir-node)
  ())

(defclass ir-push (ir-node)
  ((value :initarg :value)))

(defmethod uses-stack-p ((node ir-node))
  t)

(defclass ir-return (ir-node)
  ())

(defclass ir-return-value (ir-return)
  ((fn-name :initarg :fn-name)))

(defclass ir-sub (ir-node)
  ())

(defclass ir-throw (ir-branch)
  ())

(defclass ir-variable (ir-node)
  ((name :initarg :name)))
