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
                      collect (random 500000))))

(defvar *tables* '())
(defvar *keys* 10000000)

(defmacro run-test (name threads constructor task)
  `(progn
     (let ((the-table ,constructor)
           (threads '()))
       (let ((all-threads-keys (generate-keys ,threads (floor *keys* ,threads))))
         (present-test ,name)
         (dolist (keys all-threads-keys)
           (push (bt:make-thread
                  (lambda ()
                    (let ((start-time (get-internal-real-time)))
                      (dolist (key keys)
                        ,task)
                      (report-finish (get-internal-real-time) start-time))))
                 threads)))
       (mapc #'bt:join-thread threads)
       (push the-table *tables*))))

(defun run-tests ()
  (setf *tables* '())
  (run-test "Unsynchronised hash table, one thread"
            1
            (make-hash-table :size *keys*)
            (incf (gethash key the-table 0)))
  (run-test "Boxed hash table, one thread"
            1
            (box (make-hash-table :size *keys*))
            (with-unlocked-box (the-table the-table)
              (incf (gethash key the-table 0))))
  (run-test "Boxed hash table, five threads"
            5
            (box (make-hash-table :size *keys*))
            (with-unlocked-box (the-table the-table)
              (incf (gethash key the-table 0))))
  ;; Note that doing this is "wrong", in the sense that this won't lead to
  ;; atomic updates in either SBCL or Clozure. If two threads increment the
  ;; same value at the same time, they will leave in one piece, but the value
  ;; may only be incremented once instead of twice.
  #+sbcl
  (progn
    (run-test "Synchronised hash table, one thread"
              1
              (make-hash-table :synchronized t
                               :size *keys*)
              (incf (gethash key the-table 0)))
    (run-test "Synchronised hash table, five threads"
              5
              (make-hash-table :synchronized t
                               :size *keys*)
              (incf (gethash key the-table 0))))
  #+ccl
  (progn
    (run-test "Synchronised hash table, one thread"
              1
              (make-hash-table :shared t
                               :lock-free t
                               :size *keys*)
              (incf (gethash key the-table 0)))
    (run-test "Synchronised hash table, ten threads"
              10
              (make-hash-table :shared t
                               :lock-free t
                               :size *keys*)
              (incf (gethash key the-table 0))))
  (run-test "Concurrent hash table, one thread"
            1
            (make-chash-table :size *keys*
                              :test #'eql
                              :hash-function #'identity)
            (modify-value (key the-table)
                (old-value present?)
              (if present?
                  (values (1+ old-value) t)
                  (values 0 t))))
  (run-test "Concurrent hash table, five threads"
            5
            (make-chash-table :size *keys*
                              :test #'eql
                              :hash-function #'identity)
            (modify-value (key the-table)
                (old-value present?)
              (if present?
                  (values (1+ old-value) t)
                  (values 0 t))))
  (reverse *tables*))
