(in-package :concurrent-hash-table)

(defmacro modify-value ((key hash-table) (value present?) &body body)
  (alexandria:with-gensyms (modifier)
    `(flet ((,modifier (,value ,present?)
              ,@body))
       (declare (#+sbcl sb-int:truly-dynamic-extent
                 #-sbcl dynamic-extent #',modifier)
                (inline ,modifier))
       (modchash ,key ,hash-table #',modifier))))

(defmacro do-concurrent-table ((key value hash-table) &body body)
  (alexandria:with-gensyms (modifier)
    `(flet ((,modifier (,key ,value)
              ,@body))
       (declare (#+sbcl sb-int:truly-dynamic-extent
                 #-sbcl dynamic-extent #',modifier)
                (inline ,modifier))
       (update-chash #',modifier ,hash-table))))
