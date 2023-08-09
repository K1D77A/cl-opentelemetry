(in-package #:ot)


#||
All spans from the same trace share the same `trace_id`. 

||#
(defclass traces-data (top)
  ((trace-id
    :accessor trace-id
    :initarg :trace-id
    :documentation "Toplevel trace container for all the others.")));;no encoding at all.

(defclass spans (is-parent)
  ()
  (:default-initargs :key "spans"))

(defclass span (is-child with-attributes with-values)
  ((trace-id
    :accessor trace-id
    :initarg :trace-id
    :documentation
    "When we create a 'trace' (which is a series of span objects) we have
to set the same trace-id for them all.")
   (span-id
    :accessor span-id
    :initarg :span-id)
   (status
    :accessor status
    :initarg :status)
   (start-time
    :accessor start-time
    :initarg :start-time)
   (end-time
    :accessor end-time
    :initarg :end-time)
   (parent-span-id
    :accessor parent-span-id
    :initarg :parent-span-id    
    :initform nil
    :documentation "When there is a related parent then we set it here."))
  (:default-initargs
   :key "scope"
   :trace-id
   (random-bytes-as-hex 16)
   :span-id
   (random-bytes-as-hex 8)
   :start-time (local-time:now)
   :status "unset"))

(defmethod encode-values progn ((p span) hash)
  (with-slots (trace-id span-id start-time parent-span-id status end-time)
      p
    (setf (gethash "traceId" hash) trace-id
          (gethash "spanId" hash) span-id
          (gethash "startTimeUnixNano" hash)
          (timestamp-to-unix-nano start-time)
          (gethash "endTimeUnixNano" hash)
          (timestamp-to-unix-nano end-time)
          (gethash "status" hash) status)
    (when parent-span-id
      (setf (gethash "parentSpanId" hash) parent-span-id))))

(defclass scope (instrumentation-scope)
  ((name
    :initarg :name)
   (version
    :initarg :version))
  (:default-initargs
   :key "scope"
   :name "qtscope"
   :version "0.0.0"))

(defmethod encode-values progn ((p scope) hash)
  (with-slots (name version)
      p
    (setf (gethash "name" hash) name
          (gethash "version" version) version)))
  
(defclass scope-span (is-parent with-attributes)
  ()
  (:default-initargs :key "scopeSpans"))
;;this is some instrumentionScope or something meaning it has some values.

(defclass events (is-parent)
  ()
  (:default-initargs :key "events"))

(defclass event (is-child with-attributes with-values)
  ())

