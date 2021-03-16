(in-package :bustle)
;;; A port of https://github.com/jonhoo/bustle/blob/master/src/lib.rs#L281

(deftype percentage ()
  '(integer 0 100))

(defstruct mix
  (read   20 :type percentage)
  (insert 20 :type percentage)
  (remove 20 :type percentage)
  (update 20 :type percentage)
  (upsert 20 :type percentage))

(defstruct workload
  (mix (error "Need a mix"))
  (capacity-lb 25 :type (mod 32))
  (prefill-ratio 0.0 :type (real 0.0 1.0))
  (operations-ratio 1.0 :type real)
  (threads 4 :type (integer 1)))

(defvar *info-writer* (constantly nil))
(defun info (format-string &rest format-arguments)
  (apply *info-writer* format-string format-arguments))

(defun run (workload)
  (multiple-value-bind (total-ops spent throughput latency)
      (run-silently workload)
    (format t "~&~d operations across ~d threads in ~8e seconds; time/op = ~8e op/time = ~8e"
            total-ops
            (workload-threads workload)
            spent
            latency throughput)))

;;; We split up run_silently cause it's huge (120LOC).
(defun make-option-mix (mix)
  (assert (= (+ (mix-read mix)
                (mix-insert mix)
                (mix-remove mix)
                (mix-update mix)
                (mix-upsert mix))
             100)
          ()
          "Mix percentages do not add up to 100.")
  (macrolet ((sub-array (name)
               (let ((mix-name
                       (alexandria:symbolicate "MIX-" name)))
                 `(make-array (,mix-name mix) :initial-element ',name))))
    (alexandria:shuffle
     (concatenate 'vector
                  (sub-array read)
                  (sub-array insert)
                  (sub-array remove)
                  (sub-array update)
                  (sub-array upsert)))))

(defun next-power-of-two (n)
  (expt 2 (integer-length (1- n))))

(defun make-key-space (capacity total-ops workload)
  (let* ((mix (workload-mix workload))
         (prefill (floor (* capacity (workload-prefill-ratio workload))))
         (max-insert-ops (* (ceiling total-ops 100)
                            (+ (mix-insert mix) (mix-upsert mix))))
         (insert-keys (+ (max capacity max-insert-ops) prefill))
         (threads (workload-threads workload))
         (insert-keys-per-thread
           (next-power-of-two (ceiling insert-keys threads)))
         (keys (make-array threads)))
    (pmap (lambda (thread-id)
            (let ((key-vector (make-array insert-keys-per-thread
                                          :element-type 'fixnum))
                  (*random-state* (make-random-state t)))
              (loop for n below insert-keys-per-thread
                    do (setf (aref key-vector n)
                             (random (expt 2 62))))
              (setf (aref keys thread-id) key-vector)))
          (alexandria:iota threads))
    keys))

(defun pmap (function sequence)
  (let ((threads (map 'list
                      (lambda (element)
                        (bt:make-thread
                         (lambda ()
                           (funcall function element))))
                      sequence)))
    (mapc #'bt:join-thread threads)))

(defun prefill-table (table capacity workload key-space)
  (info "Constructing initial table.")
  (let* ((prefill (floor (* capacity (workload-prefill-ratio workload))))
         (prefill-per-thread (floor prefill (workload-threads workload))))
    (pmap (lambda (keys)
            (loop for n below prefill-per-thread
                  for key = (aref keys n)
                  do (setf (concurrent-hash-table:getchash key table) t)))
          key-space)
    prefill-per-thread))

(defun run-silently (workload)
  (let* ((capacity   (ash 1 (workload-capacity-lb workload)))
         (total-ops  (floor (* capacity (workload-operations-ratio workload))))
         (option-mix (make-option-mix (workload-mix workload)))
         (key-space  (make-key-space capacity total-ops workload))
         (table      (concurrent-hash-table:make-chash-table
                      :test #'eql
                      :hash-function #'identity
                      :size capacity))
         (prefill-per-thread (prefill-table table capacity workload key-space))
         (threads    (workload-threads workload)))
    (info "Starting test.")
    (let ((start-time (get-internal-real-time))
          (threads (loop for keys across key-space
                         collect (let ((keys keys))
                                   (bt:make-thread
                                    (lambda ()
                                      (run-mix table keys option-mix
                                               (floor total-ops threads)
                                               prefill-per-thread)))))))
      (mapc #'bt:join-thread threads)
      (let ((spent (/ (- (get-internal-real-time) start-time)
                      internal-time-units-per-second)))
        (values total-ops spent
                (/ total-ops spent)
                (/ (* spent (workload-threads workload)) total-ops))))))

(declaim (inline boolean=))
(defun boolean= (a b)
  (not (alexandria:xor a b)))
(defmacro incf/64 (place)
  `(setf ,place (ldb (byte 64 0) (1+ ,place))))

(defun run-mix (table keys ops-mix operations prefilled)
  (declare (optimize (speed 3))
           (simple-vector ops-mix)
           ((simple-array fixnum 1) keys)
           ((and unsigned-byte fixnum) operations prefilled))
  (let ((key-count (length keys))
        (erase-seq  0)
        (insert-seq prefilled)
        (find-seq   0))
    (declare (fixnum erase-seq insert-seq)
             ((unsigned-byte 64) find-seq))
    (assert (and (> key-count 4)
                 (= (logcount key-count) 1)))
    (assert (= (length ops-mix) 100))
    ;; Some kind of linear congruential generator.
    (let ((a (1+ (floor key-count 2)))
          (c (1- (floor key-count 4)))
          (mask (1- key-count))
          (operations-done 0))
      (declare ((unsigned-byte 64) a c mask operations-done))
      (macrolet ((twist-lcg (place)
                   `(setf ,place (logand mask (+ c (* a ,place))))))
        (dotimes (n (floor operations 100))
          (loop for operation across ops-mix
                do (when (= operations-done operations)
                     (return-from run-mix))
                   (incf/64 operations-done)
                   (case operation
                     ;; I can't convince myself that the tests should work.
                     ;; They even crash sometimes in the original.
                     (read
                      (concurrent-hash-table:getchash (aref keys find-seq) table)
                      (twist-lcg find-seq))
                     (insert
                      (setf (concurrent-hash-table:getchash (aref keys insert-seq)
                                                            table)
                            t)
                      (incf/64 insert-seq))
                     (remove
                      (cond
                        ((= erase-seq insert-seq)
                         (concurrent-hash-table:remchash
                          (aref keys find-seq) table)
                         (twist-lcg find-seq))
                        (t
                         (concurrent-hash-table:remchash
                          (aref keys erase-seq) table)
                         (incf/64 erase-seq))))
                     (update
                      (setf (concurrent-hash-table:getchash (aref keys find-seq)
                                                            table)
                            t)
                      (twist-lcg find-seq))
                     (upsert
                      (let ((n (min find-seq insert-seq)))
                        (twist-lcg find-seq)
                        (setf (concurrent-hash-table:getchash (aref keys n)
                                                              table)
                              t)
                        (when (= n insert-seq)
                          (incf/64 insert-seq)))))))))))
