(in-package #:ot)

(defclass opentelemetry ()
  ((traces-url
    :accessor traces-url
    :initarg :traces-url)
   (backgroundp
    :accessor backgroundp
    :initarg :backgroundp))
  (:default-initargs :backgroundp t))

(defmacro def-telemetry (name parent supers direct-slots &rest options)
  `(progn (defclass ,name ,supers
            ,(append `((parent-proto :initform
                                     ,(if parent 
                                          `(c2mop:class-prototype (find-class ',parent))
                                          `,parent)))

              direct-slots)
            ,@options)
          (c2mop:ensure-finalized (find-class ',name))))

(defgeneric add-child (parent child)
  (:method ((p with-children) child)
    (push child (children p))
    (setf (parent child) p)))

(defgeneric handle-execution-condition (opentelemetry element condition)
  (:documentation "Handle a condition from within body of with-span and with-event")
  (:method :after (ot element condition)
    (error condition)))

(defgeneric write-json (opentelemetry json &optional stream)
  (:documentation "Specialize me to export your traces. JSON is a hash-table."))

(defgeneric make-request (opentelemetry content)
  (:documentation "Specialize me to make a HTTP request.
CONTENT is the encoded JSON as a string."))
