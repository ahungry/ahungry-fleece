;;; -*-  Lisp -*-

(asdf:defsystem sb-cover
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-COVER;"
  :depends-on (sb-md5)
  :components ((:file "cover"))
  :perform (asdf:load-op :after (o c) (provide 'sb-cover))
  :perform (asdf:test-op :after (o c) (test-system 'sb-cover/tests)))

(asdf:defsystem sb-cover/tests
  #+sb-building-contrib :pathname
  #+sb-building-contrib #p"SYS:CONTRIB;SB-COVER;"
  :depends-on (sb-cover asdf)
  :components ((:file "tests")))
