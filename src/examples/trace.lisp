(in-package #:ot)

(defun test ()
  (let ((scope (make-instance 'scope))
        (resource (make-instance 'resource :attributes '(:service.name "qtservice2"))))
    (with-new-trace (*trace* *spans* :scope scope :resource resource)
                    `(:service-id "r.y.m"
                      :status ,(to-status-code :ok)
                      :service-name "demonstration"
                      :telemetry-sdk-name "cl-opentelemetry"
                      :telemetry-sdk-version ,*version* 
                      :telemetry-sdk-language "Common Lisp")
      (test2)
      (test3)
      (test4)
      (test5))))

(defun test2 ()
  (with-add-span (*trace* *spans*)
    (with-span (span :span1)
      (list :status (make-status :ok "TPD")
            :attributes '(:kn "yes")))))
       
(defun test3 ()
  (with-add-span (*trace* *spans*)
    (with-span (span :span2 :span1)
               (list :status (make-status :ok "TPD")
                     :attributes '())
      (sleep 0.25))))

(defun test4 ()
  (with-add-span (*trace* *spans*)
    (with-span (span :span3 :span2)
               (list :status (make-status :ok "TPD")
                     :attributes `())
      (sleep (random 0.5))))
  (with-add-event (*trace* *spans*)
    (dotimes (i 10)
      (with-event (event :span2)
                  (list :name (format nil "~D-weee" i)
                        :values (list :message (format nil "Test value ~D" i)))
        (sleep 0.1)))
    *trace*))


(defun test5 ()
  (handler-case 
      (with-add-span (*trace* *spans*)
        (with-span (span :span4 :span1)
                   (list :status (make-status :ok "TPD"))
          (sleep (random 0.5))
          (error "test span condition")))
    (simple-condition ()
      t))
  (with-add-event (*trace* *spans*)
    (dotimes (i 10)
      (with-event (event :span4)
                  (list :name (format nil "~D-weee" i))
        (sleep 0.1)))
    (handler-case 
        (with-event (event :span4)
                    (list :name "weeee"
                          :values '(:message "test value"))
          (sleep 0.01)
          (error "Test event error"))
      (simple-condition ()
        t))
    *trace*))

