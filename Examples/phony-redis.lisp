(defpackage :phony-redis
  (:use :cl)
  (:export #:make-server #:connect-to-server
           #:find-value #:close-connection))
(in-package :phony-redis)

(defun make-server ()
  (concurrent-hash-table:make-chash-table :test #'equal))

(defstruct connection
  send-mailbox
  response-mailbox)

(defun connect-to-server (server)
  (let ((send-mailbox     (safe-queue:make-mailbox))
        (response-mailbox (safe-queue:make-mailbox)))
    (bt:make-thread
     (lambda ()
       (loop
         (trivia:ematch (safe-queue:mailbox-receive-message send-mailbox)                 
           ((list :quit)
            (return))
           ((list :get name)
            (multiple-value-bind (value present?)
                (concurrent-hash-table:getchash name server)
              (if present?
                  (safe-queue:mailbox-send-message response-mailbox
                                                   `(:found ,value))
                  (safe-queue:mailbox-send-message response-mailbox
                                                   `(:not-found)))))
           ((list :put name value)
            (setf (concurrent-hash-table:getchash name server)
                  (copy-seq value))
            (safe-queue:mailbox-send-message response-mailbox
                                             `(:ok))))))
     :name "Phony-Redis connection thread")
    (make-connection
     :send-mailbox send-mailbox
     :response-mailbox response-mailbox)))

(defun find-value (connection name)
  (safe-queue:mailbox-send-message
   (connection-send-mailbox connection)
   `(:get ,name))
  (trivia:ematch (safe-queue:mailbox-receive-message
                  (connection-response-mailbox connection))
    ((list :found value)
     (values value t))
    ((list :not-found)
     (values nil nil))))

(defun (setf find-value) (value connection name)
  (safe-queue:mailbox-send-message
   (connection-send-mailbox connection)
   `(:put ,name ,value))
  (safe-queue:mailbox-receive-message
   (connection-response-mailbox connection)))

(defun close-connection (connection)
  (safe-queue:mailbox-send-message
   (connection-send-mailbox connection)
   `(:quit)))
