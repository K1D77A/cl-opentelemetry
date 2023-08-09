(in-package #:ot)

(defclass with-children ()
  ((children
    :accessor children
    :initarg :children
    :initform ()
    :type list)))

(defclass top (with-children)
  ())

(defclass with-key ()
  ((key
    :accessor key
    :initarg :key
    :type string)))

(defgeneric has-key-p (p)
  (:method ((p with-key))
    t)
  (:method (p)
    nil))

(defclass with-values ()
  ((kv-pairs 
    :accessor kv-pairs
    :initarg :values 
    :type list)))

(defclass is-parent (with-children with-key)
  ((parent
    :accessor parent
    :initarg :parent
    :initform nil)))

(defclass is-child ()
  ((parent
    :accessor parent
    :initarg :parent)))

(defclass with-attributes ()
  ((attributes
    :accessor attributes
    :initarg :attributes
    :initform nil)))

