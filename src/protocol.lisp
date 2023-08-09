(in-package #:to)

(defgeneric add-child (parent child)
  (:method ((p is-parent) child)
    (push child (children p))
    (setf (children p) (nreverse (children p)))
    (setf (parent child) p))
