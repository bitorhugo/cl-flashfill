;;;; SPDX-License-Identifier: MIT

(asdf:defsystem #:flashfill-tests
  :description "Tests for Flashfill"
  :author "Vitor Santos <vhsoo at proton dot me>"
  :license  "MIT"
  :version "0.0.1"
  :depends-on (:flashfill)
  :serial t
  :pathname "tests"
  :perform (test-op (op c)
		    (unless (symbol-call :flashfill '#:run-tests)
		      (error "Tests Failed.")))
  :components ((:file "utils")
	       (:file "grammar")
	       (:file "generation")
	       (:file "algorithms")))
