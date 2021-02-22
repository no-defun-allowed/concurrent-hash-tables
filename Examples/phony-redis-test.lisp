(defpackage :phony-redis-test
  (:use :cl :phony-redis)
  (:export #:run-test))
(in-package :phony-redis-test)

(defvar *names* (loop for n below 128 by 2 collect (format nil "~r" n)))
(defvar *other-names* (loop for n from 1 below 128 by 2 collect (format nil "~r" n)))
(defvar *ops* 10000000)

(defun worker (n server
               ready-semaphore start-semaphore
               writer-proportion names)
  (declare (optimize (speed 3))
           (single-float writer-proportion))
  (let ((name (elt names n))
        (bitmap (make-array 100
                            :element-type '(unsigned-byte 8)
                            :initial-element 0))
        (connection (connect-to-server server)))
    (dotimes (i 100)
      (setf (aref bitmap i)
            (if (< (random 1.0) writer-proportion)
                1
                0)))
    (bt:signal-semaphore ready-semaphore)
    (bt:wait-on-semaphore start-semaphore)
    (let ((position 0))
      (dotimes (o (the fixnum *ops*))
        (if (zerop (aref bitmap position))
            (find-value connection name)
            (setf (find-value connection name)
                  #(1)))
        (setf position (mod (1+ position) 100))))
    (close-connection connection)))

(defun test (name worker-count writer-proportion keys)
  (let* ((ready-semaphore (bt:make-semaphore :name "Ready threads"))
         (start-semaphore (bt:make-semaphore :name "Start threads"))
         (server (make-server))
         (workers (loop for n below worker-count
                        collect (let ((n n))
                                  (bt:make-thread
                                   (lambda ()
                                     (worker n server
                                             ready-semaphore start-semaphore
                                             writer-proportion
                                             keys)))))))
    (dotimes (n worker-count)
      (bt:wait-on-semaphore ready-semaphore))
    (let ((start-time (get-internal-real-time)))
      (bt:signal-semaphore start-semaphore :count worker-count)
      (mapc #'bt:join-thread workers)
      (let* ((time (float (/ (- (get-internal-real-time) start-time)
                             internal-time-units-per-second)))
             (throughput (/ (* *ops* worker-count) time)))
        (format t "~&~20@a: ~$ seconds (~10d transactions/second)"
                name time (round throughput))))))

(defun run-test ()
  (loop for workers from 1 to 10
        do (format t "~&--- ~d worker~:p ---" workers)
           (test "all writes"     workers 1.0 *names*)
           (test "all hit reads"  workers 0.0 *names*)
           (test "all miss reads" workers 0.0 *other-names*)
           (test "10% writes"     workers 0.1 *names*)
           (test "50% writes"     workers 0.5 *names*)))
