(in-package :concurrent-hash-table)

(declaim (optimize (speed 3) (safety 1)
                   (compilation-speed 0) (space 0)))

;;; A concurrent hash table, like Java's ConcurrentHashMap, that achieves some
;;; concurrency by delegating requests to one of many hash table "segments",
;;; locking each individually.

(defconstant +segments+ 64)

(defstruct (chash-table (:constructor %make-chash-table))
  #+ccl (%count 0)
  #+sbcl (%count 0 :type sb-ext:word)
  #-(or ccl sbcl) (%count (box 0) :read-only t)
  (segments (make-array +segments+) :type simple-array)
  (hash-function #'sxhash :type function))

(defun make-chash-table (&key (test #'eql)
                              (segment-hash-function #'sxhash)
                              (size 1000)
                         &allow-other-keys)
  (declare (fixnum size))
  (let* ((segments     (make-array +segments+))
         (segment-size (floor size +segments+))
         (hash-function
           (alexandria:ensure-function segment-hash-function))
         (test        (alexandria:ensure-function test))
         (chash-table (%make-chash-table
                       :segments segments
                       :hash-function hash-function)))
    (dotimes (i +segments+)
      (setf (aref segments i)
            (box (make-hash-table :test test
                                  :size segment-size))))
    chash-table))

(defmacro with-segment-held ((segment) &body body)
  (declare (ignorable segment))
  `(with-unlocked-box (,segment ,segment)
     ,@body))

(defmacro with-segment ((segment key hash-table) &body body)
  (alexandria:with-gensyms (hash segment-position)
    `(let* ((,hash (funcall (chash-table-hash-function ,hash-table) ,key))
            (,segment-position (mod ,hash +segments+))
            (,segment (svref (chash-table-segments ,hash-table)
                             ,segment-position)))
       (with-segment-held (,segment)
         ,@body))))

(declaim (inline increment-hash-table-count))
(defun increment-hash-table-count (hash-table &optional (delta 1))
  #+ccl
  (loop for old-count = (chash-table-%count hash-table)
        when (atomics:cas (chash-table-%count hash-table)
                          old-count
                          (+ old-count delta))
          return (+ old-count delta))
  #+sbcl
  (atomics:atomic-incf (chash-table-%count hash-table) delta)
  #-(or ccl sbcl)
  (with-unlocked-box (count (chash-table-%count hash-table))
    (incf count delta)))

(declaim (inline getchash (setf getchash) remchash modify-segment))
(defun getchash (key hash-table &optional default-value)
  (with-segment (segment key hash-table)
    (gethash key segment default-value)))

(defun (setf getchash) (new-value key hash-table &optional default-value)
  (declare (ignore default-value))
  (let (old-count new-count)
    (with-segment (segment key hash-table)
      (setf old-count             (hash-table-count segment)
            (gethash key segment) new-value
            new-count             (hash-table-count segment)))
    (increment-hash-table-count hash-table (- new-count old-count)))
  new-value)

(defun remchash (key hash-table)
  (let (old-count new-count value)
    (with-segment (segment key hash-table)
      (setf old-count (hash-table-count segment)
            value (remhash key segment)
            new-count (hash-table-count segment)))
    (increment-hash-table-count hash-table (- new-count old-count))
    value))

(defun modify-segment (segment key new-value old-present? new-present? hash-table)
  (if new-present?
      (progn
        (unless old-present?
          (increment-hash-table-count hash-table))
        (setf (gethash key segment) new-value))
      (if old-present?
          (progn
            (remhash key segment)
            (increment-hash-table-count hash-table -1))
          nil)))

(declaim (inline modchash mapchash))
(defun modchash (key hash-table modification-function)
  "\"Atomically\" replace the value of a key in a hash table, by calling a modification function with the old value and presence, which returns a new value and presence."
  (declare (function modification-function))
  (with-segment (segment key hash-table)
    (multiple-value-bind (old-value old-present?)
        (gethash key segment)
      (multiple-value-bind (new-value new-present?)
          (funcall modification-function old-value old-present?)
        (modify-segment segment key new-value
                        old-present? new-present?
                        hash-table)))))

(defun update-chash (function hash-table)
  (declare (function function))
  (loop for segment across (chash-table-segments hash-table)
        do (with-segment-held (segment)
             (maphash (lambda (key value)
                        (multiple-value-bind (new-value new-present?)
                            (funcall function key value)
                          (modify-segment segment key new-value
                                          t new-present?
                                          hash-table)))
                      segment))))

(defun mapchash (function hash-table)
  (loop for segment across (chash-table-segments hash-table)
        do (with-segment-held (segment)
             (maphash function segment))))

(defun chash-table-count (hash-table)
  #+(or ccl sbcl) (chash-table-%count hash-table)
  #-(or ccl sbcl)
  (with-unlocked-box (count (chash-table-%count hash-table))
    count))

(defmethod print-object ((hash-table chash-table) stream)
  (print-unreadable-object (hash-table stream :type t :identity t)
    (format stream "(~d element~:p)" (chash-table-count hash-table))))
