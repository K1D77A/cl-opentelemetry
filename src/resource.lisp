(in-package #:ot)

#||

Each class has a specific place where it can be a child.
Resource-spans - child of top
resource - child of resource-spans
noting this relationship will make inserting new children into the correct place easy

||#

(def-telemetry resource-spans top (is-parent)
  ()
  (:default-initargs :key "resourceSpans"))

(def-telemetry resource resource-spans (is-child with-attributes with-key)
  ()
  (:default-initargs :key "resource"
                     :attributes (make-resource-attributes)))

(defun make-resource-attributes (&key
                                   (name "cl-opentelemetry")
                                   (version *version*)
                                   (language "Common Lisp")
                                   (service "qtservice"))
  (list :telemetry-sdk-name name 
        :telemetry-sdk-version version
        :telemetry-sdk-language language
        :service-name service))
  
                                       

