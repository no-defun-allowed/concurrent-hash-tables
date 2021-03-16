(asdf:defsystem :bustle
  :depends-on (:concurrent-hash-tables :alexandria :bordeaux-threads)
  :serial t
  :components ((:file "package")
               (:file "bustle")
               (:file "run-tests")))
