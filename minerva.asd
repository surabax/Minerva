(defpackage :minerva
  (:use :cl))

(in-package :minerva)

(asdf:defsystem :minerva
  :description "Minerva Scheme: an implementation of R5RS Scheme"
  :version "0.1"
  :author "Yaroslav Khnygin <yaroslav.hnygin@gmail.com>"
  :licence "MIT License"
  :depends-on ("uiop")
  :components ((:file "emit")
	       (:file "data")
	       (:file "scm-bool")
	       (:file "predicates")
	       (:file "transforms")
	       (:file "forms")
	       (:file "primcalls")
	       (:file "compile")
	       (:file "tests")))
