(asdf:defsystem :concurrent-hash-tables-click
  :depends-on (:concurrent-hash-tables)
  :serial t
  :components ((:file "package")
               (:file "java-string")
               (:file "cliff-click-test")))
