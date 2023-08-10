(in-package #:ot)

(defparameter *version*
  (asdf:component-version
   (asdf:find-system "cl-opentelemetry")))

(defun timestamp-to-unix-nano (timestamp) 
 (+ (* (+ (* (+ (local-time:day-of timestamp) 11017) local-time:+seconds-per-day+)
          (local-time:sec-of timestamp))
       1000000000)
    (local-time:nsec-of timestamp)))


(defun random-bytes-as-hex (n)
  (ironclad:byte-array-to-hex-string (ironclad:random-data n)))


(defun write-json (json &optional stream)
  (let ((*print-pretty* nil))
    (shasht:write-json json stream)))
