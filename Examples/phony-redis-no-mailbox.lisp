;;; According to a presentation by an Amazon Web Services engineer,
;;; one cannot design an in-memory database using a garbage collected
;;; language implementation, because it would be too slow due to
;;; collection.  He then goes to use a hash table with just one lock,
;;; finds performance similar to Redis, and calls it a day.

;;; Using structure sharing and a concurrent hash table, we can go
;;; much faster.  Perhaps a magnitude or two faster - not that I'm
;;; really taking the problem seriously, by avoiding network
;;; serialisation, and even inter-thread mailboxes; but this should
;;; show that tasteful use of concurrent data structures makes things
;;; go fast.

(defpackage :phony-redis
  (:use :cl)
  (:export #:make-server #:connect-to-server
           #:find-value #:close-connection))
(in-package :phony-redis)

(defun make-server ()
  (concurrent-hash-table:make-chash-table :test #'equal))

(defun connect-to-server (server)
  server)

(defun find-value (connection name)
  (concurrent-hash-table:getchash name connection))

(defun (setf find-value) (value connection name)
  (setf (concurrent-hash-table:getchash name connection)
        value))

(defun close-connection (connection)
  (declare (ignore connection))
  (values))
