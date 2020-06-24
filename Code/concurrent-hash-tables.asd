(asdf:defsystem :concurrent-hash-tables
  :author "The Cooperative of Applied Language"
  :depends-on (:bordeaux-threads #+(or ccl sbcl) :atomics)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               #-ccl (:file "segmented")
               #+ccl (:file "clozure")
               (:file "imperative")
               (:file "benchmark")))
