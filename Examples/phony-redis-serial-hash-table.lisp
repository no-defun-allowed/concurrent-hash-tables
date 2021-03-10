(defpackage :phony-redis
  (:use :cl)
  (:export #:make-server #:connect-to-server
           #:find-value #:close-connection))
(in-package :phony-redis)

(defun make-server ()
  (list
   (bt:make-lock)
   (make-hash-table :test #'equal)))

(defun connect-to-server (server)
  server)

(defun find-value (connection name)
  (bt:with-lock-held ((first connection))
    (gethash name (second connection))))

(defun (setf find-value) (value connection name)
  (bt:with-lock-held ((first connection))
    (setf (gethash name (second connection))
          value)))

(defun close-connection (connection)
  (declare (ignore connection))
  (values))
