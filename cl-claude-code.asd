(defsystem "cl-claude-code"
  :depends-on ("alexandria"
               "yason"
               "async-process"
               "sb-concurrency"
               "bordeaux-threads")
  :serial t
  :components ((:file "claude-code")))
