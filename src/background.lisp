(in-package #:ot)

#||
We want a background thread to handle encode/decode, converting to json and submitting.
The overhead for spawning a new thread seems so miniscule that our #'fire function can just
spawn a new thread.
||#


(defclass opentelemetry ()
  ((traces-url
    :accessor traces-url
    :initarg :traces-url)))

(defparameter *ot*
  (make-instance 'opentelemetry
                 :traces-url *trace-url*))


(defgeneric record (opentelemetry top)
  (:documentation "Record that you fired a TOP."))

(defmethod record ((ot opentelemetry) top)
  (format *debug-io* "Sending trace: ~A to ~A~%" (trace-id top) (traces-url ot)))

(defun fire (opentelemetry top)
  "Takes in some tree, encodes it and sends it."
  (record opentelemetry top)
;;  (bt2:make-thread
  ;; (lambda ()
     (let* ((hash (make-hash-table :test #'equal)))
       (encode top hash)
       (dex:post (traces-url opentelemetry)
                 :headers '(("Content-Type" . "application/json"))
                 :content (write-json hash))))
   ;;:initial-bindings `((opentelemetry . ,opentelemetry))))


  
  




