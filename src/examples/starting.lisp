(in-package #:ot)

(defclass my-opentelemetry (opentelemetry)
  ())

(setf *ot* (make-instance 'my-opentelemetry
                          :backgroundp t
                          :traces-url "my-traces-url"))
;;need to have dexador
(defmethod make-request ((ot my-opentelemetry) content)
  ;;content is the json as a string
  (dex:post (traces-url ot)
            :headers '(("Content-Type" . "application/json"))
            ;;^ must be application/json 
            :content content))

;;need to have shasht 
(defmethod write-json ((ot my-opentelemetry) json &optional stream)
  (let ((*print-pretty* nil))
    (shasht:write-json json stream)))


