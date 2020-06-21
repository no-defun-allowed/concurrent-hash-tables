(in-package :concurrent-hash-table)

;;; A simplified locked-box from decentralise2-utilities.

(defstruct locked-box
  value
  (lock (bt:make-lock "locked-box lock") :read-only t))

(defmacro with-unlocked-box ((value box) &body body)
  "A box that allows access to its value, by only one thread at a time.

  (defvar *box* (make-locked-box :value 42))

  (with-thread (:name \"Evil fighting thread!\")
    (loop
      (setf v :woohoo-type-error-time)
      (sleep 0.01)))

  (with-unlocked-box (v *box*)
    (setf v 12300)
    (+ v 45)) ; => always 12345"
  (alexandria:once-only (box)
    `(bt:with-lock-held ((locked-box-lock ,box))
       (symbol-macrolet ((,value (locked-box-value ,box)))
         . ,body))))

(defun box (initial-value)
  (make-locked-box :value initial-value))
