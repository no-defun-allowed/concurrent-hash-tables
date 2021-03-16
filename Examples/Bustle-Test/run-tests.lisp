(in-package :bustle)
(defvar *rapid-grow-mix*
  (make-mix
   :read 5
   :insert 80
   :remove 5
   :update 10
   :upsert 0))

(defun rapid-grow (threads)
  (make-workload
   :mix *rapid-grow-mix*
   :threads threads
   :capacity-lb 24
   :prefill-ratio 0.0))

(defvar *exchange-mix*
  (make-mix
   :read 10
   :insert 40
   :remove 40
   :update 10
   :upsert 0))

(defun exchange (threads)
  (make-workload
   :mix *rapid-grow-mix*
   :threads threads
   :capacity-lb 24
   :prefill-ratio 0.8))

(defvar *cache-mix*
  (make-mix
   :read 98
   :insert 1
   :remove 1
   :update 0
   :upsert 0))

(defun cache (threads)
  (make-workload
   :mix *cache-mix*
   :threads threads
   :capacity-lb 24
   :prefill-ratio 0.8))

(defvar *cliff-mix*
  (make-mix
   :read 50
   :insert 25
   :remove 25
   :update 0
   :upsert 0))

(defun cliff (threads)
  (make-workload
   :mix *cache-mix*
   :threads threads
   :capacity-lb 16
   :prefill-ratio 1.0
   :operations-ratio 2000.0))

(defun run-test (name maximum-threads)
  (loop for threads from 1 to maximum-threads
        do (format t "~&~:(~a~) with ~d thread~:p" name threads)
           (run (funcall name threads))))

(defun run-tests (&key (maximum-thread-count 12))
  (flet ((run-threads (name)
           (loop for threads from 1 to maximum-thread-count
                 do (run-test name threads))))
    (run-threads 'rapid-grow)
    (run-threads 'exchange)
    (run-threads 'cache)
    (run-threads 'click)))
