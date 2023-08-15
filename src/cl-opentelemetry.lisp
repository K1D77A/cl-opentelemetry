(in-package #:ot)

(defparameter *ot*
  (make-instance 'opentelemetry
                 :traces-url *trace-url*
                 :backgroundp t))

