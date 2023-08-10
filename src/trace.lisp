(in-package #:ot)


#||
All spans from the same trace share the same `trace_id`. 

||#

(def-telemetry traces-data nil (top)
  ((trace-id
    :accessor trace-id
    :initarg :trace-id
    :type (or null string)
    :documentation "Toplevel trace container for all the others.")
   (store
    :accessor store
    :initform ()
    :type list
    :documentation "A plist of named elements."))
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
              
(def-telemetry span spans (is-child with-children with-attributes with-values
                                    with-unique-id)
  ((name
    :accessor name
    :initarg :name
    :initform nil
    :type (or null string))
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
    :initform nil
    :type (or null string))
   (status
    :accessor status
    :initarg :status
    :initform ()
    :type list)
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
   :status '(:code 1 :message "λ Made with alien tech λ")))


(defmethod encode-values progn ((p span) hash)
  (with-slots (trace-id span-id start-time parent-span-id status end-time name children)
      p
    (setf (gethash "traceId" hash) trace-id
          (gethash "spanId" hash) span-id
          (gethash "startTimeUnixNano" hash)
          (timestamp-to-unix-nano start-time)
          (gethash "endTimeUnixNano" hash)
          (timestamp-to-unix-nano end-time)
          (gethash "name" hash) name
          (gethash "error" hash) "true"
          (gethash "kind" hash) 2)
    (when children
      (setf (gethash "events" hash)
            (mapcar (lambda (ev)
                      (encode-values ev (make-hash-table :test #'equal)))
                    children)))              
    (when status
      (setf (gethash "status" hash)
            (let ((hash (make-hash-table :test #'equal)))
              (setf (gethash "status" hash) (getf status :code)
                    (gethash "message" hash) (getf status :message))
              hash)))                          
    (when parent-span-id
      (setf (gethash "parentSpanId" hash) parent-span-id))))



(def-telemetry events span (is-parent)
  ()  
  (:default-initargs :key "events"))

(def-telemetry event events (is-child with-attributes with-values)
  ())
  

(defun new-trace (resource-init scope-init)
  (let ((traces (make-instance 'traces-data))
        (resource-span (make-instance 'resource-spans))
        (resource (apply #'make-instance 'resource resource-init))
        (scope-span (make-instance 'scope-span))
        (spans (make-instance 'spans))
        (scope (apply #'make-instance 'scope scope-init)))
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

(defmacro with-new-trace ((trace-val spans-val &key resource-init scope-init) &body body)
  `(let ((,trace-val (new-trace ,resource-init ,scope-init))
         (,spans-val ()))
     (declare (special ,trace-val ,spans-val))
     (locally ,@body)))

(defmacro with-trace-fun ((trace-val spans-val) &body body)
  `(locally 
       (declare (special ,trace-val ,spans-val))
     (flet ((add-span (unique-id span-inits &optional parent-span-unique-id)
              (declare (special ,trace-val ,spans-val))
              (when (and (boundp ',trace-val)
                         (boundp ',spans-val))
                (check-type unique-id keyword)
                (let* ((parent? (when parent-span-unique-id
                                  (getf ,spans-val parent-span-unique-id)))
                       (span (apply #'make-instance 'span 
                                    (list* :trace-id (trace-id ,trace-val)
                                           :parent-span-id (and parent?;;check for parent? 
                                                                (span-id parent?))
                                           :name (str:dot-case unique-id)
                                           span-inits))))
                  (setf (getf ,spans-val unique-id) span)
                  (add-child (getf (store ,trace-val) :spans) span)
                  span)))
            (get-span (unique-id)
              (declare (special ,trace-val ,spans-val))
              (when (and (boundp ',trace-val)
                         (boundp ',spans-val))
                (getf ,spans-val unique-id)))
            (add-event (parent-span-unique-id &key attributes values)
              (declare (special ,trace-val ,spans-val))
              ;;need to add 'events'
              (when (and (boundp ',trace-val)
                         (boundp ',spans-val))
              (let ((parent (getf ,spans-val parent-span-unique-id))
                    (instance (make-instance 'event
                                             :attributes attributes
                                             :values values)))
                (add-child parent instance)
                instance))))
       (locally ,@body))))

(defmacro with-span ((span-var unique-id initargs &optional parent-span-unique-id)
                     &body body)
  `(let* ((,span-var (add-span ,unique-id ,initargs ,parent-span-unique-id)))            
     (prog1 (locally ,@body)
       (setf (end-time ,span-var) (local-time:now)))))

(defun test ()
  (with-new-trace (*trace* *spans*)
    (test2)
    (test3)
    (test4)))

(defun test2 ()
  (with-trace-fun (*trace* *spans*)
    (with-span (span :n (list :status '(:code 2 :message "TPD")
                              :attributes
                              '(:kn "yes"
                                :service-id "r.y.m"
                                :status 2
                                :service-name "test name")))
      
      (sleep 1))))


(defun test3 ()
  (with-trace-fun (*trace* *spans*)
    (with-span (span
                :n2
                (list :status '(:code 2 :message "TPD")
                      :attributes '(:service-id "r.y.m"
                                    :status 2
                                    :service-name "test name"))
                :n)
      (sleep 0.25))))

(defun test4 ()
  (with-trace-fun (*trace* *spans*)
    (dotimes (i 1)
      (with-span (span
                  (intern (string (gensym)) :keyword)
                  (list :status '(:code 2 :message "message")
                        :attributes `(:service-id "r.y.m"
                                      :scope.name "TND"
                                      :status 2
                                      :error "true"
                                      :error-name "599"
                                      :error-level "warn"
                                      :error-message "yes"
                                      :service-name "test name"
                                      :telemetry-sdk-name "cl-opentelemetry"
                                      :telemetry-sdk-version ,*version* 
                                      :telemetry-sdk-language "Common Lisp"))
                  :n2)
        (sleep (random 0.5))
        (add-event :n2 :attributes '(:name "smc"
                                     :error.0.level "warn"
                                     :error.0.message "weee"
                                     :error.0.name "TND"))))
    *trace*))

(defun test-fire ()
  (let* ((hash (make-hash-table :test #'equal))
         (test (test))
         (encoded (encode test hash)))
    (shasht:write-json hash t)
    (dex:post *traces*
              :headers '(("Content-Type" . "application/json"))
              :content (shasht:write-json hash nil))))

                      
