(asdf:defsystem :concurrent-hash-tables
  :author "The Cooperative of Applied Language"
  :depends-on (:bordeaux-threads #+(or ccl sbcl) :atomics)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "segmented")
               (:file "benchmark")))
