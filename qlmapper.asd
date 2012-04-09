;;;; qlmapper.asd

(asdf:defsystem #:qlmapper
  :serial t
  :depends-on (#:alexandria)
  :description "For each system or release in Quicklisp, run some code
  by loading a specific file."
  :author "Zach Beane <xach@xach.com>"
  :license "MIT"
  :components ((:file "package")
               (:file "qlmapper")))

