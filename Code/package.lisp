(defpackage :concurrent-hash-table
  (:use :cl)
  (:export #:make-chash-table
           #:getchash #:remchash 
           #:chash-table-count
           #:mapchash #:modify-value
           #:modchash
           #:update-chash #:do-concurrent-table
           #:run-tests))
