(asdf:defsystem :typed
  :serial t
  :pathname #P"src/"
  :components
  ((:file "typed"))
  :depends-on (:cl-ppcre :cl-annot))
