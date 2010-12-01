;; -*- Mode:Common-lisp -*- fv.ast

(defsystem fv
  :version "0.1"
  :description "VAT invoicing program for the Polish tax system."
  :depends-on (cl-emb)
  :serial t
  :components ((:file "fv")
	       (:file "menu")
	       (:file "polish")
	       (:file "naggum")))
