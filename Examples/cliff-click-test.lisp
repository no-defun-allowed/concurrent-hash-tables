;;; A port of perf_hash_test.java from high-scale-lib.

(defvar *read-ratio*)
(defvar *gr*)
(defvar *pr*)

(defvar *thread-min*)
(defvar *thread-max*)
(defvar *thread-increment*)
(defvar *table-size*)
(defmacro volatile-setf (&rest args)
  `(sb-thread:barrier (:memory)
     (setf ,@args)))

(defvar *keys*)
;;; remember to do atomic writes on *start* and *stop*
(defvar *start*)
(defvar *stop*)
(defvar *cpus* 12)

(defun check (x msg lower upper)
  (when (or (< x lower) (> x upper))
    (error "~a must be from ~d to ~d" msg lower upper))
  x)

(defun run-test (read-ratio
                 thread-min thread-max thread-incr
                 table-size)
  ;; "Parse" args
  (setf *read-ratio* read-ratio
        *thread-min* thread-min
        *thread-max* thread-max
        *thread-increment* thread-incr
        *table-size* table-size)
  (setf *gr* (floor (ash *read-ratio* 20) 100)
        *pr* (+ (floor (- (ash 1 20) *gr* 2) *gr*)))
  (let ((trips (floor (- *thread-max* *thread-min*) *thread-increment*)))
    (setf *thread-max* (+ (* trips *thread-increment*) *thread-min*)))
  (format t "~&~d% gets, ~d% inserts, ~d% removes, table size = ~d"
          *read-ratio*
          (floor (- 100 *read-ratio*) 2)
          (floor (- 100 *read-ratio*) 2)
          *table-size*)
  (format t "~&Threads from ~d to ~d by ~d"
          *thread-min* *thread-max* *thread-increment*)
  (let ((keymax 1))
    (loop while (< keymax *table-size*)
          do (setf keymax (ash keymax 1)))
    (setf *keys* (make-array keymax))
    (loop for n below (length *keys*)
            do (setf (aref *keys* n)
                     (format nil "~Dabc~D"
                             n (+ (* n 17) 123))))
    (loop for threads from *thread-min* to *thread-max* by *thread-increment*
          do (run-till-stable threads 7))))

(defun string-hash (string)
  (declare (simple-string string)
           (optimize (speed 3)))
  (let ((hash 7))
    (loop for char across string
          do (setf hash
                   (ldb (byte 32 0)
                        (+ (* hash 31) (char-code char)))))
    hash))

(defun run-till-stable (threads trials)
  (let ((table (concurrent-hash-table:make-chash-table
                :size 1024
                :test #'equal
                :hash-function #'string-hash)))
    (format t "~&=== ~3d" threads)
    ;; Quicky sanity check
    (loop for n below 100
          do (setf (concurrent-hash-table:getchash (aref *keys* n)
                                                   table)
                   (aref *keys* n))
             (loop for m below n
                   unless (equal (concurrent-hash-table:getchash (aref *keys* n)
                                                                 table)
                                 (aref *keys* n))
                     do (error "Broken table, put ~d but cannot find #~d"
                               n m)))
    (let ((results (make-array trials))
          (total  0))
      (loop for j below trials
            for ops    = (make-array threads)
            for nanos  = (make-array threads)
            for millis = (run-once threads table ops nanos)
            for sum-ops   = (reduce #'+ ops)
            for sum-nanos = (reduce #'+ nanos)
            for ops-per-sec = (floor (* sum-ops 1000) millis)
            do (setf (aref results j) ops-per-sec)
               (incf total ops-per-sec)
               (when (zerop j)
                 (format t "  cnts/sec="))
               (format t " ~10d" ops-per-sec))
      ;; Print standard deviation and hash table stats?
      )))

(defun run-once (threads table ops nanos)
  (volatile-setf *start* nil
                 *stop*  nil)
  (setf (concurrent-hash-table:getchash "Hayley" table)
        "Hayley")
  (concurrent-hash-table:remchash "Hayley" table)
  (loop while (< (+ (concurrent-hash-table:chash-table-count table)
                    1024)
                 *table-size*)
        do (loop with idx = (random (expt 2 31))
                 for i below 1024
                 for key = (aref *keys*
                                 (logand idx (1- (length *keys*))))
                 do (setf (concurrent-hash-table:getchash key table) key)))
  ;; Launch threads
  (let ((threads
          (loop for n below threads
                collect (let ((n n))
                          (bt:make-thread
                           (lambda ()
                             (worker-run n table ops nanos))))))
        (start-time (get-internal-real-time)))
    (volatile-setf *start* t)
    (sleep 2)
    (volatile-setf *stop* t)
    (let* ((stop-time (get-internal-real-time))
           (millis  (/ (- stop-time start-time)
                       (/ internal-time-units-per-second 1000))))
      (mapc #'bt:join-thread threads)
      millis)))

(defun worker-run (n table ops nanos)
  (let ((state (random (expt 2 64)))
        (get-ops 0)
        (put-ops 0)
        (del-ops 0)
        (start-time (get-internal-real-time)))
    (flet ((next-random ()
             (let ((c (ldb (byte 32 32) state))
                   (x (ldb (byte 32 0)  state)))
               (setf state (ldb (byte 64 0) (+ c (* x 4294883355))))
               (logxor c x))))
      (declare (inline next-random)
               (dynamic-extent #'next-random))
      (loop until *start*)
      (loop until *stop*
            for x   = (ldb (byte 20 0) (next-random))
            for key = (aref *keys* (logand (next-random) (1- (length *keys*))))
            do (cond
                 ((< x *gr*)
                  (incf get-ops)
                  (let ((value (concurrent-hash-table:getchash key table)))
                    (assert (or (null value) (string= value key)))))
                 ((< x *pr*)
                  (incf put-ops)
                  (setf (concurrent-hash-table:getchash key table)
                        key))
                 (t
                  (incf del-ops)
                  (concurrent-hash-table:remchash key table))))
      (setf (aref ops n)
            (+ get-ops put-ops del-ops)
            (aref nanos n)
            (/ (- (get-internal-real-time) start-time)
               (/ internal-time-units-per-second 1.0e9))))))
