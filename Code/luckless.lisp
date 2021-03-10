(cl:in-package :cl)

(defpackage :concurrent-hash-table
  (:use :cl)
  (:export #:make-chash-table
           #:getchash #:remchash 
           #:chash-table-count
           #:mapchash #:modify-value
           #:modchash
           #:update-chash #:do-concurrent-table
           #:run-tests)
  (:local-nicknames (#:luckless #:org.shirakumo.luckless.hashtable)))

(in-package :concurrent-hash-table)

;;; Basically the Clozure implementation, but using Shinmera's
;;; Luckless <https://github.com/Shinmera/luckless>, which is a port of
;;; Cliff Click's NonBlockingHashMap to Common Lisp.

;;; And it might get confused if you don't use my fork
;;; <https://github.com/no-defun-allowed/luckless> because I implemented
;;; MAPHASH there.

(defun make-chash-table (&rest r
                         &key (test #'eql)
                              hash-function
                              (size 1000)
                         &allow-other-keys)
  (declare (ignore test hash-function size))
  (apply #'luckless:make-castable :allow-other-keys t r))

(declaim (inline getchash (setf getchash)
                 remchash modchash))
(defun getchash (key hash-table &optional default-value)
  (luckless:gethash key hash-table default-value))

(defun (setf getchash) (new-value key hash-table &optional default-value)
  (declare (ignore default-value))
  (setf (luckless:gethash key hash-table) new-value))

(defun remchash (key hash-table)
  (luckless:remhash key hash-table))

(defun modchash (key hash-table modification-function)
  (declare (function modification-function)
           (luckless:castable hash-table))
  (tagbody
   try-again
     (multiple-value-bind (old-value present?)
         (luckless:gethash key hash-table)
       (if present?
           (multiple-value-bind (new-value new-present?)
               (funcall modification-function
                        old-value t)
             (if new-present?
                 ;; Replace the value.
                 (unless (luckless:put-if-equal hash-table
                                                key
                                                new-value old-value)
                   (go try-again))
                 ;; Remove the value.
                 (unless (luckless:try-remhash hash-table
                                               key
                                               old-value)
                   (go try-again))))
           (multiple-value-bind (new-value new-present?)
               (funcall modification-function nil nil)
             (if new-present?
                 ;; Create a new box.
                 (unless (luckless:put-if-absent hash-table
                                                 key
                                                 new-value)
                   (go try-again))
                 ;; Do nothing.
                 nil))))))

(declaim (inline update-chash mapchash chash-table-count))
(defun update-chash (function hash-table)
  (declare (function function)
           (luckless:castable hash-table))
  (luckless:maphash
   (lambda (key value)
     (declare (ignore value))
     (modchash key hash-table
               (lambda (old-value present?)
                 (if present?
                     (funcall function key old-value)
                     (values nil nil)))))
   hash-table))

(defun mapchash (function hash-table)
  (luckless:maphash function hash-table))

(defun chash-table-count (hash-table)
  (luckless:count hash-table))
