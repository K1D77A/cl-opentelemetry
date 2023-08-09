(in-package #:ot)

(defclass resource-spans (is-parent)
  ()
  (:default-initargs :key "resourceSpans"))

(defclass resource (is-child with-attributes with-key)
  ()
  (:default-initargs :key "resource"))
