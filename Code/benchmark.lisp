(in-package :concurrent-hash-table)

;;; Testing the throughput of the new concurrent hash table.
;;; In the test, each thread is given 1,000,000 keys to increment
;;; the values of.

(defun present-test (name)
  (format *debug-io* "~&Testing ~a:~%" name))

(defvar *report-lock* (bt:make-lock))
(defun report-finish (end-time start-time)
  (bt:with-lock-held (*report-lock*)
    (format *debug-io* "~&  Finished in ~$ seconds~%"
            (/ (- end-time start-time)
               internal-time-units-per-second))))

(defun generate-keys (threads keys-per-thread)
  (loop repeat threads
        collect (loop repeat keys-per-thread
                      collect (random (expt 2 32)))))

(defmacro run-test (name threads constructor task)
  `(progn
     (present-test ,name)
     (let ((the-table ,constructor)
           (threads '()))
       (let ((all-threads-keys (generate-keys ,threads 1000000)))
         (dolist (keys all-threads-keys)
           (push (bt:make-thread
                  (lambda ()
                    (let ((start-time (get-internal-real-time)))
                      (dolist (key keys)
                        ,task)
                      (report-finish (get-internal-real-time) start-time))))
                 threads)))
       (mapc #'bt:join-thread threads)
       the-table)))

(defun run-tests ()
  (run-test "Unsynchronised hash table, one thread"
            1
            (make-hash-table :size 1000000)
            (incf (gethash key the-table 0)))
  (run-test "Boxed hash table, one thread"
            1
            (box (make-hash-table :size 1000000))
            (with-unlocked-box (the-table the-table)
              (incf (gethash key the-table 0))))
  (run-test "Boxed hash table, ten threads"
            10
            (box (make-hash-table :size 1000000))
            (with-unlocked-box (the-table the-table)
              (incf (gethash key the-table 0))))
  #+sbcl
  (progn
    (run-test "Synchronised hash table, one thread"
              1
              (make-hash-table :synchronized t
                               :size 1000000)
              (incf (gethash key the-table 0)))
    (run-test "Synchronised hash table, ten threads"
              10
              (make-hash-table :synchronized t
                               :size 1000000)
              (incf (gethash key the-table 0))))
  (run-test "Concurrent hash table, one thread"
            1
            (make-chash-table :size 1000000
                              :segment-hash-function #'identity)
            (modchash key the-table
                      (lambda (old-value present?)
                        (if present?
                            (values (1+ old-value) t)
                            (values 0 nil)))))
  (run-test "Concurrent hash table, ten threads"
            10
            (make-chash-table :size 1000000
                              :segment-hash-function #'identity)
            (modchash key the-table
                      (lambda (old-value present?)
                        (if present?
                            (values (1+ old-value) t)
                            (values 0 t))))))
