(in-package #:ot)

#||
We want a background thread to handle encode/decode, converting to json and submitting.
The overhead for spawning a new thread seems so miniscule that our #'fire function can just
spawn a new thread.
||#



(defgeneric record (opentelemetry top)
  (:documentation "Record that you fired a TOP."))

(defmethod record (ot top)
  (format *debug-io* "Sending trace: ~A to ~A. Backgroundp: ~A.~%"
          (trace-id top) (traces-url ot) (backgroundp ot)))

(defun fire (opentelemetry top)
  "Takes in some tree, encodes it and sends it."
  (record opentelemetry top)
  (flet ((doit ()
           (let* ((hash (make-hash-table :test #'equal)))
             (encode top hash)
             (dex:post (traces-url opentelemetry)
                       :headers '(("Content-Type" . "application/json"))
                       :content (write-json hash)))))
    (if (backgroundp opentelemetry)
        (bt2:make-thread
         (lambda ()
           (doit)))
        (doit))))


  
  




