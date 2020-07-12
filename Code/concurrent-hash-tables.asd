(asdf:defsystem :concurrent-hash-tables
  :author "The Cooperative of Applied Language"
  :depends-on (:bordeaux-threads #+(or ccl sbcl) :atomics)
  :serial t
  :components ((:file "package")
               (:file "utilities")
               #.(cond
                   ((member :ccl *features*)
                    '(:file "clozure"))
                   #+(or)
                   ((asdf:find-system "luckless" nil)
                    '(:file "luckless"))
                   (t '(:file "segmented")))
               (:file "imperative")
               (:file "benchmark")))
