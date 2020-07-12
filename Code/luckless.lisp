(in-package :concurrent-hash-table)

;;; Basically the Clozure implementation, but using Shinmera's
;;; Luckless <https://github.com/Shinmera/luckless>, which is a port of
;;; Cliff Click's NonBlockingHashMap to Common Lisp.

;;; And it might get confused if you don't use my fork
;;; <https://github.com/no-defun-allowed/luckless> because I implemented
;;; MAPHASH there.

(defun make-chash-table (&key (test #'eql)
                              (hash-function #'sxhash)
                              (size 1000)
                         &allow-other-keys)
  (luckless-hashtable:make-castable :test test
                                    :hash-function hash-function
                                    :size size))

(defmacro with-assurance-we-have-a-simple-vector ((variable) &body body)
  `(locally
       (declare ((simple-vector 1) ,variable)
                (optimize (speed 3) (safety 0)))
     ,@body))

(declaim (inline getchash (setf getchash)
                 remchash modchash))
(defun getchash (key hash-table &optional default-value)
  (let ((value-box (luckless-hashtable:gethash key hash-table)))
    (if (null value-box)
        (values default-value nil)
        (with-assurance-we-have-a-simple-vector (value-box)
          (values (svref value-box 0) t)))))

(defun (setf getchash) (new-value key hash-table &optional default-value)
  (declare (ignore default-value))
  (setf (luckless-hashtable:gethash key hash-table)
        (vector new-value)))

(defun remchash (key hash-table)
  (luckless-hashtable:remhash key hash-table))

(defun modchash (key hash-table modification-function)
  (declare (function modification-function)
           (luckless-hashtable:castable hash-table))
  (tagbody
   try-again
     (multiple-value-bind (old-value-box present?)
         (luckless-hashtable:gethash key hash-table)
       (if present?
           (with-assurance-we-have-a-simple-vector (old-value-box)
             (let ((old-value (svref old-value-box 0)))
               (multiple-value-bind (new-value new-present?)
                   (funcall modification-function
                            old-value t)
                 (if new-present?
                     ;; Replace the value in the box.
                     (unless (atomics:cas (svref old-value-box 0)
                                          old-value new-value)
                       (go try-again))
                     ;; Remove the box.
                     (luckless-hashtable:remhash key hash-table)))))
           (multiple-value-bind (new-value new-present?)
               (funcall modification-function nil nil)
             (if new-present?
                 ;; Create a new box.
                 (setf (luckless-hashtable:gethash key hash-table)
                       (vector new-value))
                 ;; Do nothing.
                 nil))))))

(declaim (inline update-chash mapchash chash-table-count))
(defun update-chash (function hash-table)
  (declare (function function)
           (luckless-hashtable:castable hash-table))
  (luckless-hashtable:maphash
   (lambda (key value)
     (declare (ignore value))
     (modchash key hash-table
               (lambda (old-value present?)
                 (declare (ignore present?))
                 (funcall function key old-value))))
   hash-table))
(defun mapchash (function hash-table)
  (luckless-hashtable:maphash (lambda (key value)
                                (funcall function key (svref value 0)))
                              hash-table))

(defun chash-table-count (hash-table)
  (luckless-hashtable:count hash-table))
