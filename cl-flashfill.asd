;;;; cl-flashfill.asd

(asdf:defsystem #:cl-flashfill
  :description "Flashfill in Common Lisp"
  :author "Vitor Santos <vhsoo at proton dot me>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "util")
	       (:file "flashfill")))
