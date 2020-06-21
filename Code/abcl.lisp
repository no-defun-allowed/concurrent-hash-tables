(in-package :concurrent-hash-table)

;;; On ABCL, we may as well use Java's ConcurrentHashMap.  

;;; Some stuff to make Java FFI go fast, by resolving methods at load-time.
(defmacro method (class name &rest arguments)
  `(load-time-value (java:jmethod ,class ,name ,@arguments)))
(defmacro call (class name subject &rest arguments)
  `(java:jcall (method ,class ,name ,@(mapcar #'second arguments))
               ,subject ,@(mapcar #'first arguments)))
(defmacro call-raw (class name subject &rest arguments)
  `(java:jcall-raw (method ,class ,name ,@(mapcar #'second arguments))
                   ,subject ,@(mapcar #'first arguments)))

(defvar *bifunction*
  (java:jnew-runtime-class
   "LispBiFunction"
   :interfaces '("java.util.function.BiFunction")
   :fields '(("function" "org.armedbear.lisp.LispObject"))
   :constructors '((("org.armedbear.lisp.LispObject")
                    (lambda (this function)
                      (setf (java:jfield "function" this) function))))
   :methods '(("apply" "java.lang.Object"
               ("java.lang.Object" "java.lang.Object")
               '(lambda (this argument1 argument2)
                 (funcall (java:jfield "function" this)
                  argument1 argument2))))))
(defun bifunction (function)
  (java:jnew *bifunction* function))

(declaim (inline null-pointer-p))
(defun null-pointer-p (value)
  (and (java:java-object-p value)
       (java:jnull-ref-p value)))

(defun make-chash-table (&key (test #'eql)
                              (segment-hash-function #'sxhash)
                              (segment-size 1000))
  (declare (ignore test segment-hash-function segment-size))
  (java:jnew "java.util.concurrent.ConcurrentHashMap"))

(declaim (inline getchash getchash remchash modchash mapchash))
(defun getchash (key hash-table &optional default-value)
  (let ((value (call-raw "java.util.concurrent.ConcurrentHashMap"
                         "get" hash-table (key "java.lang.Object"))))
    (if (null-pointer-p value)
        (values default-value nil)
        (values value t))))

(defun (setf getchash) (new-value key hash-table &optional default-value)
  (declare (ignore default-value))
  (call "java.util.concurrent.ConcurrentHashMap"
        "put" hash-table
        (key       "java.lang.Object")
        (new-value "java.lang.Object"))
  new-value)

(defun remchash (key hash-table)
  (let ((value (call-raw "java.util.concurrent.ConcurrentHashMap"
                         "remove" hash-table
                         (key "java.lang.Object"))))
    (null-pointer-p value)))

(defun chash-table-count (hash-table)
  (call "java.util.concurrent.ConcurrentHashMap"
        "size" hash-table))

(defun modchash (key hash-table modification-function)
  (call "java.util.concurrent.ConcurrentHashMap"
        "compute" hash-table
        (key "java.lang.Object")
        ((bifunction
             (lambda (key value)
               (declare (ignore key))
               (multiple-value-bind (value present?)
                   (if (null-pointer-p value)
                       (funcall modification-function nil   nil)
                       (funcall modification-function value t))
                 (if present?
                     value
                     java:+null+))))
         "java.util.function.BiFunction")))
