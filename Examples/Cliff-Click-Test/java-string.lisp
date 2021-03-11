(in-package :cliff-click-test)
;;; Emulate Java's String class, specifically how it caches the hash value as
;;; Java strings are immutable.
(defun string-hash (string)
  (declare (simple-string string)
           (optimize (speed 3)))
  (let ((hash 7))
    (loop for char across string
          do (setf hash
                   (ldb (byte 32 0)
                        (+ (* hash 31) (char-code char)))))
    hash))

(defstruct (java-string (:constructor %make-java-string))
  string
  (hash 0 :type (unsigned-byte 32)))

(defun java-string (lisp-string)
  (let ((hash (string-hash lisp-string)))
    (%make-java-string
     :string lisp-string
     :hash hash)))
