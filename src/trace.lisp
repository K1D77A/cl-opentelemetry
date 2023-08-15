(in-package #:ot)

#||
Implementing tracing in a nice way
||#

(def-telemetry traces-data nil (top)
  ((trace-id
    :accessor trace-id
    :initarg :trace-id
    :type (or null string)
    :documentation "Toplevel trace container for all the others.")
   (spans
    :accessor spans
    :initarg :spans
    :initform ()
    :type list
    :documentation "plist of span ids to spans made for this trace."))
  (:default-initargs
   :trace-id (random-bytes-as-hex 16)));;no encoding at all.

(def-telemetry scope-spans resource-spans (is-parent with-attributes)
  ()
  (:default-initargs :key "scopeSpans"))
;;this is some instrumentionScope or something meaning it has some values.

(def-telemetry scope scope-spans (instrumentation-scope)
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
          (gethash "version" hash) version)))
  
(def-telemetry spans scope-spans (is-parent)
  ()
  (:default-initargs :key "spans"))

(defun to-kind (symbolic)
  (check-type symbolic keyword)
  (ecase symbolic
    (:unspecific 0)
    (:internal 1)
    (:server 2)
    (:client 3)
    (:producer 4)
    (:consumer 5)))

(def-telemetry span spans (is-child with-attributes with-values
                                    with-unique-id)
  ((name
    :accessor name
    :initarg :name
    :initform nil
    :type (or null string))
   (events
    :accessor events
    :initarg :events
    :initform nil 
    :type (or null events))
   (trace-id
    :accessor trace-id
    :initarg :trace-id
    :documentation
    "When we create a 'trace' (which is a series of span objects) we have
to set the same trace-id for them all.")
   (span-id
    :accessor span-id
    :initarg :span-id)
   (kind
    :accessor kind
    :initarg :kind
    :initform :server
    :type keyword)
   (status
    :accessor status
    :initarg :status
    :type hash-table)
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
   :trace-id
   (random-bytes-as-hex 16)
   :span-id
   (random-bytes-as-hex 8)
   :start-time (local-time:now)
   :kind :server
   :events nil
   :status (make-status :ok "λ Made with alien tech λ")))

(defun to-status-code (symbolic)
  (check-type symbolic keyword)
  (ecase symbolic
    (:unset 0)
    (:ok 1)
    (:error 2)))

(defun make-status (code message)
  (check-type code keyword)
  (check-type message string)
  (let ((hash (make-hash-table :test #'equal :size 2)))
    (setf (gethash "code" hash)
          (to-status-code code)
          (gethash "message" hash)
          message)
    hash))

(defmethod encode-values progn ((p span) hash)
  (with-slots (trace-id span-id start-time
               parent-span-id status end-time
               name events kind)
      p
    (setf (gethash "traceId" hash) trace-id
          (gethash "spanId" hash) span-id
          (gethash "startTimeUnixNano" hash)
          (timestamp-to-unix-nano start-time)
          (gethash "endTimeUnixNano" hash)
          (timestamp-to-unix-nano end-time)
          (gethash "name" hash) name
          (gethash "error" hash) "true"
          (gethash "kind" hash) (to-kind kind))
    (when events
      (setf (gethash "events" hash)
            (mapcar (lambda (ev)
                      (encode-values ev (make-hash-table :test #'equal)))
                    (children events))))
    (when status
      (setf (gethash "status" hash) status))
    (when parent-span-id
      (setf (gethash "parentSpanId" hash) parent-span-id))))

(def-telemetry events span (is-parent)
  ()  
  (:default-initargs :key "events"))

(def-telemetry event events (is-child with-attributes with-values)
  ((start-time
    :accessor start-time
    :initarg :start-time
    :type local-time:timestamp)
   (name
    :accessor name
    :initarg :name
    :type string)))

(defmethod encode-values progn ((p event) hash)
  (with-slots (name start-time)
      p
    (setf (gethash "name" hash) name
          (gethash "timeUnixNano" hash)
          (timestamp-to-unix-nano start-time))))

(defun new-trace (resource scope &optional shared-attributes)
  (check-type resource resource)
  (check-type scope scope)
  (let ((traces (make-instance 'traces-data :shared-attributes shared-attributes))
        (resource-span (make-instance 'resource-spans))
        (scope-span (make-instance 'scope-spans))
        (spans (make-instance 'spans)))
    (with-accessors ((store store))
        traces 
      (setf (getf store :resource-span) resource-span
            (getf store :scope-span) scope-span
            (getf store :spans) spans) 
      (add-child traces resource-span)
      (add-child resource-span resource)
      (add-child resource-span scope-span)
      (add-child scope-span scope)
      (add-child scope-span spans)
      traces)))

(defmacro with-new-trace ((trace-val &key (resource (make-instance 'resource))
                                       (scope (make-instance 'scope)))
                          shared-attributes
                          &body body)
  "Initiate a new trace. TRACE-VAL is a symbol that is declared special
and are then used by the #'with-add-* macros in order to properly associated/place certain
objects. SHARED-ATTRIBUTES is a plist of keyword -> values that are shared between every
 instance of span and event created for this new trace."
  `(let ((,trace-val (new-trace ,resource ,scope ,shared-attributes)))
     (declare (special ,trace-val))
     (unwind-protect (locally ,@body)
       (fire *ot* ,trace-val))))

(defmacro with-add-span ((trace-val) &body body)
    "Given appropriate TRACE-VAL make a function #'add-span available in BODY.
This function correctly adds an instance of 'span to the trace and sets the correct parent
for the span when the parents ID is provided."
  `(flet ((add-span (unique-id span-inits &optional parent-span-unique-id)
            (declare (special ,trace-val))
            (when (boundp ',trace-val)
              (check-type unique-id keyword)
              (setf (getf span-inits :attributes)
                    (append (shared-attributes ,trace-val)
                            (getf span-inits :attributes)))
              (let* ((parent? (when parent-span-unique-id
                                (getf (spans ,trace-val) parent-span-unique-id)))
                     (span (apply #'make-instance 'span 
                                  (list* :trace-id (trace-id ,trace-val)
                                         :parent-span-id (and parent?;;check for parent? 
                                                              (span-id parent?))
                                         :name (str:dot-case unique-id)
                                         span-inits))))
                (setf (getf (spans ,trace-val) unique-id) span)
                (add-child (getf (store ,trace-val) :spans) span)
                span))))
     (declare (special ,trace-val))
     (locally ,@body)))

(defmacro with-add-event ((trace-val) &body body)
  "Given appropriate TRACE-VAL make a function #'add-event available in BODY.
This function correctly adds an instance of 'event to the correct span."
  `(flet ((add-event (parent-span-unique-id initargs)
            (declare (special ,trace-val))
            (when (boundp ',trace-val)
              (check-type parent-span-unique-id keyword)
              (let ((parent (getf (spans ,trace-val) parent-span-unique-id)))
                (with-accessors ((events events))
                    parent
                  (setf (getf initargs :attributes)
                        (append (shared-attributes ,trace-val)
                                (getf initargs :attributes)))
                  (let ((instance (apply #'make-instance 'event initargs)))
                    (setf (start-time instance) (local-time:now))
                    (unless events 
                      (let ((ev (make-instance 'events :parent parent)))
                        (setf events ev)))
                    (add-child events instance)
                    instance))))))
     (declare (special ,trace-val))
     (locally ,@body)))

(defmethod handle-execution-condition (opentelemetry (s span) condition)
  (let ((cs (safe-format ("~~Cannot output message~~") nil "~A" condition))
        (a (attributes s)))
    (reinitialize-instance
     s
     :status (make-status :error "Condition Signalled")
     :attributes (append (list :error-name
                               (format nil "~A" (class-name (class-of condition)))
                               :error-message cs)
                         a))))

(defmethod handle-execution-condition (opentelemetry (e event) condition)
  (let ((cs (safe-format ("~~Cannot output message~~") nil "~A" condition))
        (a (attributes e)))
    (reinitialize-instance
     e
     :attributes (append (list :error-name
                               (format nil "~A" (class-name (class-of condition)))
                               :error-message cs)
                         a))))   

(defmacro with-span ((span-var unique-id &optional parent-span-unique-id)
                      span-initargs
                     &body body)
  (check-type parent-span-unique-id (or null keyword))
  `(let ((,span-var (add-span ,unique-id ,span-initargs ,parent-span-unique-id)))
     (handler-case
         (unwind-protect (locally ,@body)
           (setf (end-time ,span-var) (local-time:now)))
       (serious-condition (c)
         (handle-execution-condition *ot* ,span-var c)))))

(defmacro with-event ((event-var parent-id)
                      initargs
                      &body body)
  (check-type parent-id keyword)
  `(let ((,event-var (add-event ,parent-id ,initargs)))
     (declare (ignorable ,event-var))
     (handler-case (locally ,@body)
       (serious-condition (c)
         (handle-execution-condition *ot* ,event-var c)))))



