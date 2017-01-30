(asdf:defsystem :typed-test
  :serial t
  :pathname #P"test/"
  :components
  ((:file "typed"))
  :depends-on (:fiveam)
  :perform (asdf:test-op (o c)
             (asdf-utils:symbol-call :fiveam :run! :typed)))
