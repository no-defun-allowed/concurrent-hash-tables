(in-package :concurrent-hash-table)

(defun make-chash-table (&rest rest
                         &key test hash-function size
                         &allow-other-keys)
  (declare (ignore test hash-function size))
  (apply #'threadmill:make-hash-table rest))
  
(defun getchash (key hash-table &optional (default nil))
  (threadmill:gethash key hash-table default))

(defun (setf getchash) (new-value key hash-table &optional default)
  (declare (ignore default))
  (setf (threadmill:gethash key hash-table) new-value))

(defun remchash (key hash-table)
  (threadmill:remhash key hash-table))

(defun modchash (key hash-table modification-function)
  (threadmill:modhash key hash-table modification-function))

(defun mapchash (hash-table function)
  (threadmill:maphash hash-table function))

(declaim (inline update-chash))
(defun update-chash (function hash-table)
  (declare (function function))
  (threadmill:maphash
   (lambda (key value)
     (declare (ignore value))
     (modchash key hash-table
               (lambda (old-value present?)
                 (if present?
                     (funcall function key old-value)
                     (values nil nil)))))
   hash-table))

(defun chash-table-count (hash-table)
  (threadmill:hash-table-count hash-table))

(defun implementation ()
  :threadmill)
