(in-package #:ot)

(defclass telemetry-class ()
  ((parent-proto
    :reader parent-proto
    :allocation :class
    :type (or null standard-object))))

(defclass with-children (telemetry-class)
  ((children
    :accessor children
    :initarg :children
    :initform ()
    :type list)))

(defgeneric has-children-p (node)
  (:method ((p with-children))
    t)
  (:method (p)
    nil))

(defclass top (with-children)
  ((store
    :accessor store
    :initform ()
    :type list
    :documentation "A plist of named elements.")
   (shared-attributes
    :accessor shared-attributes
    :initarg :shared-attributes
    :initform ()
    :type list
    :documentation "A plist of attributes shared between all children that have atts.")))

(c2mop:ensure-finalized (find-class 'top))

(defgeneric topp (node)
  (:method ((p top))
    t)
  (:method (p)
    nil))

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
    :initform nil
    :type list)))

(defclass is-parent (with-children with-key telemetry-class)
  ((parent
    :accessor parent
    :initarg :parent
    :initform nil)
   (parent-class
    :accessor parent-class
    :initarg :parent-class
    :initform nil
    :type (or null class))))

(defclass is-child (telemetry-class)
  ((parent
    :accessor parent
    :initarg :parent)))

(defclass with-attributes ()
  ((attributes
    :accessor attributes
    :initarg :attributes
    :initform nil)))

(defclass with-unique-id ()
  ((id
    :accessor id
    :initarg :id
    :type keyword
    :documentation "A lisp ID used to find an element.")))
