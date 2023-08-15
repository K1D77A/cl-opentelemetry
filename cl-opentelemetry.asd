;;;; cl-opentelemetry.asd

(defparameter *version* "0.0.2")

(asdf:defsystem #:cl-opentelemetry
  :description "Implementing some of the Opentelemetry API"
  :author "K1D77A"
  :license  "MIT"
  :version #.*version*
  :depends-on (#:alexandria
               #:local-time
               #:ironclad
               #:bordeaux-threads
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
               (:file "trace")
               (:file "background")
               (:file "cl-opentelemetry")))
