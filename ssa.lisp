(in-package :openldk)

(defclass ssa-node ()
  ((pc-index :initarg :pc-index)))

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

(defclass ssa-call (ssa-node)
  ())

(defclass ssa-branch-target (ssa-node)
  ((index :initarg :index)))

(defclass ssa-call-special-method (ssa-call)
  ((class-name :initarg :class-name)
   (method-name :initarg :method-name)
   (args :initarg :args :initform nil)))

(defclass ssa-call-virtual-method (ssa-call)
  ((method-name :initarg :method-name)
   (args :initarg :args :initform nil)))

(defclass ssa-call-static-method (ssa-call-virtual-method)
  ((class-name :initarg :class-name)))

(defclass ssa-clinit (ssa-call)
  ((class-name :initarg :class-name)))

(defclass ssa-static-member (ssa-node)
  ((class-name :initarg :class-name)
   (member-name :initarg :member-name)))

(defclass ssa-store (ssa-node)
  ((target :initarg :target)))

(defclass ssa-if (ssa-node)
  ((condition :initarg :condition)
   (branch-if-true :initarg :branch-if-true)
   (branch-if-false :initarg :branch-if-false)))

(defclass ssa-new (ssa-node)
  ((class-name :initarg :class-name)))

(defclass ssa-mul (ssa-node)
  ())

(defclass ssa-pop (ssa-node)
  ())

(defclass ssa-push (ssa-node)
  ((value :initarg :value)))

(defclass ssa-return (ssa-node)
  ())

(defclass ssa-sub (ssa-node)
  ())

(defclass ssa-throw (ssa-branch)
  ())

(defclass ssa-variable (ssa-node)
  ((name :initarg :name)))
