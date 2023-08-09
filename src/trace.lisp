(in-package #:ot)


#||
All spans from the same trace share the same `trace_id`. 

||#
(defclass traces-data (is-parent)
  ());;no encoding at all.

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
   (ironclad:byte-array-to-hex-string (ironclad:random-data 16))
   :span-id
   (ironclad:byte-array-to-hex-string (ironclad:random-data 8))
   :start-time (local-time:now)
   :status "unset"))

(defclass scope (instrumentation-scope)
  ()
  (:default-initargs
   :key "scope"))

  
(defmethod encode-values progn ((p span) hash)
  (with-slots (trace-id span-id start-time parent-span-id status end-time)
      p
    (setf (gethash "trace_id" hash) trace-id
          (gethash "span_id" hash) span-id
          (gethash "start_time_unix_nano" hash)
          (timestamp-to-unix-nano start-time)
          (gethash "end_time_unix_nano" hash)
          (timestamp-to-unix-nano end-time)
          (gethash "status" hash) status)
    (when parent-span-id
      (setf (gethash "parent_span_id" hash) parent-span-id))))

(defclass scope-span (is-parent with-attributes)
  ()
  (:default-initargs :key "scopeSpans"))
;;this is some instrumentionScope or something meaning it has some values.

    
(defclass events (is-parent)
  ()
  (:default-initargs :key "events"))

(defclass event (is-child with-attributes with-values)
  ())

