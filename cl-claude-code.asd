(defsystem "cl-claude-code"
  :depends-on ("alexandria"
               "yason"
               "async-process"
               "lem-mailbox")
  :serial t
  :components ((:file "claude-code")))
