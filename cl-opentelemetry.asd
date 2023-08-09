;;;; cl-opentelemetry.asd

(asdf:defsystem #:cl-opentelemetry
  :description "Implementing some of the Opentelemetry API"
  :author "K1D77A"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (#:shasht
               #:dexador
               #:alexandria
               #:ironclad)
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "cl-opentelemetry")))
