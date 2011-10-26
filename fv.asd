;;;; fv.asd

(asdf:defsystem #:fv
  :version "0.1"
  :description "VAT invoicing program for the Polish tax system."
  :serial t
  :depends-on (#:cl-emb
               #:local-time)
  :components ((:file "package")
               (:file "naggum")
               (:file "menu")
               (:file "fv")))
