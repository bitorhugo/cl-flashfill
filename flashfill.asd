;;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:flashfill
  :description "Flashfill in Common Lisp"
  :author "Vitor Santos <vhsoo at proton dot me>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :pathname "src"
  :in-order-to ((test-op (test-op "flashfill-tests")))
  :components ((:file "package")
               (:file "utils")
	       (:file "grammar")
	       (:file "generation")
	       (:file "algorithms")))
