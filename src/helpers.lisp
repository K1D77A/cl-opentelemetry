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

(defmacro safe-format ((fail-phrase) destination control-string &rest format-arguments)
  `(handler-case
       (format ,destination ,control-string ,@format-arguments)
     (serious-condition ()
       ,fail-phrase)))
