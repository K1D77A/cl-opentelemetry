(in-package #:to)

(defclass instrumentation-scope (is-child with-attributes with-values with-key)
  ((name
    :accessor name
    :initarg :name
    :initform nil)
   (version
    :accessor version
    :initarg :version
    :initform nil)))

(defmethod encode-values progn ((p instrumentation-scope) hash)
  (with-slots (name version)
      p
    (when name
      (setf (gethash "name" hash) name))
    (when version
      (setf (gethash "version" hash) version))))
