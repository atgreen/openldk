(in-package :openldk)

(defclass ssa-node ()
  ((pc-index :initarg :pc-index)))

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

(defclass ssa-assign (ssa-node)
  ((target :initarg :target)
   (source :initarg :source)))

(defclass ssa-add (ssa-node)
  ())

(defclass ssa-branch (ssa-node)
  ((offset :initarg :offset)))

(defclass ssa-div (ssa-node)
  ())

(defclass ssa-dup (ssa-node)
  ())

(defclass ssa-goto (ssa-branch)
  ())

(defclass ssa-if-icmple (ssa-branch)
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

(defclass ssa-instanceof (ssa-node)
  ((class :initarg :class)))

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

(defclass ssa-mul (ssa-node)
  ())

(defclass ssa-pop (ssa-node)
  ())

(defclass ssa-push (ssa-node)
  ((value :initarg :value)))

(defclass ssa-return (ssa-node)
  ())

(defclass ssa-return-value (ssa-node)
  ((fn-name :initarg :fn-name)))

(defclass ssa-sub (ssa-node)
  ())

(defclass ssa-throw (ssa-branch)
  ())

(defclass ssa-variable (ssa-node)
  ((name :initarg :name)))
