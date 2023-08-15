(in-package #:ot)

(defun test ()
  (let ((scope (make-instance 'scope))
        (resource (make-instance 'resource :attributes '(:service.name "qtservice2"))))
    (with-new-trace (*trace* :scope scope :resource resource)
                    `(:service-id "r.y.m"                      
                      :service-name "demonstration"
                      :telemetry-sdk-name "cl-opentelemetry"
                      :telemetry-sdk-version ,*version* 
                      :telemetry-sdk-language "Common Lisp")
      (test2)
      (test3)
      (test4)
      (test5))))

(defun test2 ()
  (with-add-span (*trace*)
    (with-span (span :span1);our first span with the unique id :span1
      (list :status (make-status :ok "TPD")
            :attributes '(:kn "yes")))))
       
(defun test3 ()
  (with-add-span (*trace*)
    (with-span (span :span2 :span1);span1 is the parent. This becomes a subspan
               (list :status (make-status :ok "TPD")
                     :attributes '())
      (sleep 0.25))))

(defun test4 ()
  (with-add-span (*trace*)
    (with-span (span :span3 :span2);span2 is the parent, this is another subspan
               (list :status (make-status :ok "TPD");status is a slot for span
                     :attributes `());attributes is a slot for span
      (sleep (random 0.5))))
  (with-add-event (*trace*)
    (dotimes (i 10)
      (with-event (event :span2);all of these events are added into :span2
                  (list :name (format nil "~D-weee" i)
                        :values (list :message (format nil "Test value ~D" i)))
        (sleep 0.1)))
    *trace*))


(defun test5 ()
  (handler-case 
      (with-add-span (*trace*)
        (with-span (span :span4 :span1)
                   ;;despite being all over the shop we are still able to add span4 to span1 
                   (list :status (make-status :ok "TPD"));status is a slot
          (sleep (random 0.5))
          (error "test span condition")))
    (simple-condition ()
      t));these errors are being handled just to suppress the debugger when trying this at repl
  (with-add-event (*trace*)
    (dotimes (i 10)
      (with-event (event :span4)
                  (list :name (format nil "~D-weee" i));name is a slot in events
        (sleep 0.1)))
    (handler-case 
        (with-event (event :span4)
                    (list :name "weeee";name is a slot
                          :values '(:message "test value"));event has values.
          (sleep 0.01)
          (error "Test event error"))
      (simple-condition ()
        t))
    *trace*))

