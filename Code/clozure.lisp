(in-package :concurrent-hash-table)

(declaim (optimize (speed 3) (safety 1)))

;;; Piggy back off Clozure's lock-free hash tables.
;;; To allow for atomic updating, each value is a simple-vector containing
;;; the actual value.

(defun make-chash-table (&key (test #'eql)
                              segment-hash-function
                              (size 1000))
  (declare (ignore segment-hash-function))
  (make-hash-table :test test :size size
                   :lock-free t
                   :shared t))

(defmacro with-assurance-we-have-a-simple-vector ((variable) &body body)
  `(locally
       (declare ((simple-vector 1) ,variable)
                (optimize (speed 3) (safety 0)))
     ,@body))

(declaim (inline getchash (setf getchash)
                 remchash modchash))
(defun getchash (key hash-table &optional default-value)
  (let ((value-box (gethash key hash-table)))
    (if (null value-box)
        (values default-value nil)
        (with-assurance-we-have-a-simple-vector (value-box)
          (values (svref value-box 0) t)))))

(defun (setf getchash) (new-value key hash-table &optional default-value)
  (declare (ignore default-value))
  (setf (gethash key hash-table)
        (vector new-value)))

(defun remchash (key hash-table)
  (remhash key hash-table))

(defun modchash (key hash-table modification-function)
  (declare (function modification-function)
           (hash-table hash-table))
  (tagbody
   try-again
     (multiple-value-bind (old-value-box present?)
         (gethash key hash-table)
       (if present?
           (with-assurance-we-have-a-simple-vector (old-value-box)
             (let ((old-value (svref old-value-box 0)))
               (multiple-value-bind (new-value new-present?)
                   (funcall modification-function
                            old-value present?)
                 (if new-present?
                     ;; Replace the value in the box.
                     (unless (atomics:cas (svref old-value-box 0)
                                          old-value new-value)
                       (go try-again))
                     ;; Remove the box.
                     (remhash key hash-table)))))
           (multiple-value-bind (new-value new-present?)
               (funcall modification-function nil nil)
             (if new-present?
                 ;; Create a new box.
                 (setf (gethash key hash-table)
                       (vector new-value))
                 ;; Do nothing.
                 nil))))))

(declaim (inline update-chash mapchash chash-table-count))
(defun update-chash (function hash-table)
  (declare (function function)
           (hash-table hash-table))
  (maphash (lambda (key value)
             (declare (ignore value))
             (modchash key hash-table
                       (lambda (old-value present?)
                         (declare (ignore present?))
                         (funcall function key old-value))))
           hash-table))
(defun mapchash (function hash-table)
  (maphash (lambda (key value)
             (funcall function key (svref value 0)))
           hash-table))

(defun chash-table-count (hash-table)
  (hash-table-count hash-table))
