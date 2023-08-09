(in-package #:ot)

(defun timestamp-to-unix-nano (timestamp) 
  (+ (* (+ (* (+ (local-time:day-of timestamp) 11017) local-time:+seconds-per-day+)
           (local-time:sec-of timestamp))
        1000)
     (local-time:nsec-of timestamp)))

