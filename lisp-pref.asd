(asdf:defsystem #:lisp-pref
  :description "Preference system for Common Lisp"
  :license "Apache"
  :serial t
  :depends-on (:alexandria)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "lisp-pref")))))
