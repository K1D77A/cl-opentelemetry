;;;; cl-opentelemetry.asd

(defparameter *version* "0.0.1")

(asdf:defsystem #:cl-opentelemetry
  :description "Implementing some of the Opentelemetry API"
  :author "K1D77A"
  :license  "MIT"
  :version #.*version*
  :depends-on (#:shasht
               #:dexador
               #:alexandria
               #:local-time
               #:ironclad
               #:str)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "helpers")
               (:file "mixins")
               (:file "encode")
               (:file "protocol")
               (:file "common")
               (:file "resource")
               (:file "trace")))
