(in-package :cliff-click-test)
;;; A port of perf_hash_test.java from high-scale-lib.

(declaim ((unsigned-byte 32) *gr* *pr*))
(sb-ext:defglobal *read-ratio* 0)
(sb-ext:defglobal *gr* 0)
(sb-ext:defglobal *pr* 0)

(defvar *thread-min*)
(defvar *thread-max*)
(defvar *thread-increment*)
(defvar *table-size*)
(defmacro volatile-setf (&rest args)
  `(sb-thread:barrier (:memory)
     (setf ,@args)))

(declaim (simple-vector *keys*))
(sb-ext:defglobal *keys*  #())
;;; remember to do atomic writes on *start* and *stop*
(sb-ext:defglobal *start* nil)
(sb-ext:defglobal *stop*  nil)

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
        *pr* (+ (floor (- (ash 1 20) *gr*) 2) *gr*))
  (let ((trips (floor (- *thread-max* *thread-min*) *thread-increment*)))
    (setf *thread-max* (+ (* trips *thread-increment*) *thread-min*)))
  (format t "~&~d% gets, ~d% inserts, ~d% removes, table size = ~d"
          *read-ratio*
          (floor (- 100 *read-ratio*) 2)
          (floor (- 100 *read-ratio*) 2)
          *table-size*)
  (format t "~&Threads from ~d to ~d by ~d on ~:(~a~)"
          *thread-min* *thread-max* *thread-increment*
          (concurrent-hash-table:implementation))
  (let ((keymax 1))
    (loop while (< keymax *table-size*)
          do (setf keymax (ash keymax 1)))
    (setf *keys* (make-array keymax))
    (loop for n below (length *keys*)
          do (setf (aref *keys* n)
                   (java-string
                    (format nil "~Dabc~D"
                            n (+ (* n 17) 123)))))
    (format t "~&=== ~17t ")
    (dotimes (n 7)
      (format t " ~10d" n))
    (format t " ~10@a" "average")
    (loop for threads from *thread-min* to *thread-max* by *thread-increment*
          do (run-till-stable threads 7))))

(defun standard-deviation (results)
  (let ((mean (/ (reduce #'+ results) (length results))))
    (sqrt (/ (loop for result across results
                   sum (expt (- result mean) 2))
             (length results)))))

(defun run-till-stable (threads trials)
  ;; Fortunately, we re-use String objects, so EQUAL won't barf when given
  ;; JAVA-STRING instances.
  (let ((table (concurrent-hash-table:make-chash-table
                :size (length *keys*)
                :test #'equal
                :hash-function #'java-string-hash
                :resize-threshold 0.5)))
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
      (format t " ~10d (σ = ~10d)"
              (round total trials)
              (standard-deviation results)))))

(defun setup-table (table)
  (let ((hayley-string (java-string "Hayley")))
    (setf (concurrent-hash-table:getchash hayley-string table)
          hayley-string)
    (concurrent-hash-table:remchash hayley-string table))
  (loop while (< (+ (concurrent-hash-table:chash-table-count table)
                    1024)
                 *table-size*)
        do (loop with idx = (random (expt 2 31))
                 for i below 1024
                 for key = (aref *keys*
                                 (logand idx (1- (length *keys*))))
                 do (setf (concurrent-hash-table:getchash key table) key)
                    (incf idx))))

(defun run-once (threads table ops nanos)
  (volatile-setf *start* nil
                 *stop*  nil)
  (setup-table table)
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
    (unwind-protect
         (let* ((stop-time (get-internal-real-time))
                (millis  (/ (- stop-time start-time)
                            (/ internal-time-units-per-second 1000))))
           (mapc #'bt:join-thread threads)
           millis)
      (volatile-setf *stop* t))))

(defconstant +multiplier+ #x5DEECE66D)
(defconstant +addend+ #xB)
(defconstant +mask+ (1- (ash 1 48))) 

(defun worker-run (n table ops nanos)
  (let ((seed    (random (expt 2 64)))
        (get-ops 0)
        (put-ops 0)
        (del-ops 0)
        (start-time (get-internal-real-time)))
    (declare ((unsigned-byte 64) seed)
             ((unsigned-byte 32) get-ops put-ops del-ops)
             (optimize (speed 3)
                       #+sbcl
                       (sb-c::insert-array-bounds-checks 0)))
    (macrolet ((incf/32 (place)
                 `(setf ,place (ldb (byte 32 0) (1+ ,place)))))
      (flet ((next-random ()
               (let ((next-seed (logand +mask+ (+ (* seed +multiplier+) +addend+))))
                 (setf seed next-seed)
                 (ash next-seed -17))))
        (declare (inline next-random)
                 (dynamic-extent #'next-random))
        (loop until *start*)
        (loop with keys = *keys*
              with length-1 = (1- (length keys))
              with gr = *gr*
              with pr = *pr*
              until *stop*
              for x   = (ldb (byte 20 0) (next-random))
              for key = (aref keys (logand (next-random) length-1))
              do (cond
                   ((< x gr)
                    (incf/32 get-ops)
                    (let ((value (concurrent-hash-table:getchash key table)))
                      (assert (or (null value)
                                  (eq key value)))))
                   ((< x pr)
                    (incf/32 put-ops)
                    (setf (concurrent-hash-table:getchash key table)
                          key))
                   (t
                    (incf/32 del-ops)
                    (concurrent-hash-table:remchash key table))))
        (setf (aref ops n)
              (+ get-ops put-ops del-ops)
              (aref nanos n)
              (/ (- (get-internal-real-time) start-time)
                 (/ internal-time-units-per-second 1.0e9)))))))
