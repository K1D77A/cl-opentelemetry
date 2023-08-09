(in-package #:ot)

(defgeneric add-child (parent child)
  (:method ((p with-children) child)
    (push child (children p))
    (setf (children p) (nreverse (children p)))
    (setf (parent child) p)))

(defmacro with-children ((parent) &rest body)
  `(let ((bruh ,@body))
     (add-child ,parent bruh)))

(defun start-trace ()
  (make-instance 'traces-data :trace-id (random-bytes-as-hex 16)))

