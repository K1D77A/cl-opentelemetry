(in-package #:ot)

(defmacro def-telemetry (name parent supers direct-slots &rest options)
  `(progn (defclass ,name ,supers
            ,(append `((parent-proto :initform
                                     ,(if parent 
                                          `(c2mop:class-prototype (find-class ',parent))
                                          `,parent)))

              direct-slots)
            ,@options)
          (c2mop:ensure-finalized (find-class ',name))))

;;I dont think I will use this

;; (defun find-children-of-type-node (root node)
;;   "Try to find all of NODE in ROOT."
;;   (if (has-children-p root)
;;       (if (eq (class-of root)
;;               (class-of node))
;;           node
;;           (when (children root)
;;             (or (find (class-of node) (children root) :test #'eq :key #'class-of)
;;                 (loop :for child :in (children root)
;;                       :do (let ((found? (find-children-of-type-node child node)))
;;                             (when found?
;;                               (return-from find-children-of-type-node found?)))))))
;;       (when (eq (class-of root)
;;                 (class-of node))
;;         node)))

(defgeneric add-child (parent child)
  (:method ((p with-children) child)
    (push child (children p))
    (setf (parent child) p)))


;;what we actually want is something similar where we create the toplevel
;;the whole this is a big object. so we start like so

(defparameter *traces* ())

(defun start-trace ()
  (make-instance 'traces-data :trace-id (random-bytes-as-hex 16)))

