(asdf:defsystem :concurrent-hash-tables
  :author "The Cooperative of Applied Language"
  :depends-on (:bordeaux-threads #+(or ccl sbcl) :atomics)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               #.(cond
                   ;; The Clozure hash table appears to scale miserably
                   ;; with multiple threads??
                   #+(or)
                   ((member :ccl *features*)
                    '(:file "clozure"))
                   ((find-package '#:threadmill)
                    '(:file "threadmill"))
                   ((find-package '#:org.shirakumo.luckless.hashtable)
                    '(:file "luckless"))
                   (t '(:file "segmented")))
               (:file "imperative")
               (:file "benchmark")))
