(in-package #:ot)

(defgeneric encode (node hash)
  (:documentation "Encode NODE to a hash-table.")
  (:method ((n top) hash)
    (encode (first (children n)) hash)))

(defmethod encode ((n is-parent) hash)
  (when (children n)
    (setf (gethash (key n) hash)
          (mapcar (lambda (child)
                    (let ((hash (make-hash-table :test #'equal)))
                      (encode child hash)
                      hash))
                  (reverse (children n))))))

(defmethod encode ((n is-child) hash)
  ;;if it has a name then we encode (gethash <name> ..)
  ;;otherwise we make a new hash and encode the values into that.
  (if (has-key-p n)
      (setf (gethash (key n) hash)
            (encode-values n (make-hash-table :test #'equal)))
      (encode-values n hash)))
               
(defun choose-type-name (val)
  (typecase val 
    (string "stringValue")
    (integer "intValue")
    (float "doubleValue")
    (boolean "boolValue")))

;;https://github.com/open-telemetry/opentelemetry-proto/blob/main/examples/logs.json
;;would have to do something special for array and map value
;;here is the link they *DONT* have on their website
    

(defgeneric encode-values (node hash)
  (:method-combination progn :most-specific-first))

(defparameter *dots* (make-hash-table :test #'eq))

(declaim (inline encode-to-dots))
(defun encode-to-dots (key)
  (check-type key keyword)
  (or (gethash key *dots*)
      (setf (gethash key *dots*) (str:dot-case key))))
  
(defmethod encode-values progn ((p with-attributes) hash)
  (let ((atts ()))
    (when (attributes p)
      (alexandria:doplist (key val (attributes p))
        (let ((hash (make-hash-table :test #'equal)))
          (setf (gethash "key" hash) (encode-to-dots key)
                (gethash "value" hash)
                (let ((h (make-hash-table :test #'equal)))
                  (setf (gethash (choose-type-name val) h) val)
                  h))
          (push hash atts)))
      (setf (gethash "attributes" hash) atts)
      hash)))

(defparameter *camels* (make-hash-table :test #'eq))

(declaim (inline encode-to-camel))
(defun encode-to-camel (key)
  (check-type key keyword)
  (or (gethash key *camels*)
      (setf (gethash key *camels*) (str:camel-case key))))

(defmethod encode-values progn ((p with-values) hash)
  (alexandria:doplist (key val (kv-pairs p))
    (setf (gethash (encode-to-camel key) hash) val))
  hash)
      
                                   
