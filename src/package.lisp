;;;; package.lisp

(defpackage #:cl-opentelemetry
  (:use #:cl)
  (:nicknames #:ot)
  (:export #:*ot*
           #:with-event
           #:with-span
           #:handle-execution-condition
           #:with-add-event
           #:with-add-span
           #:with-new-trace
           #:telemetry-class
           #:opentelemetry
           #:traces-url
           #:backgroundp
           #:record
           #:handle-failed-request
           #:fire
           #:encode
           #:write-json
           ))
