;;;; SPDX-License-Identifier: MIT

(in-package :flashfill)

(deftest test-literal ()
  "Tests `literal' programs."
  (check
    (string= (literal nil) nil)
    (not (string= (literal nil) ""))
    (string= (literal "") "")
    (string= (literal "a") "a")
    (string= (literal "@") "@")))

(deftest test-sub-str ()
  "Tests `sub-sub' programs."
  (let ((target "Foo Bar"))
    (check
      (string= (sub-str target 0 3) "Foo")
      (string= (sub-str target 0 3 :from-end t) "Bar")
      (string= (sub-str target 0 (length target)) target)
      (string= (sub-str target 0 (length target) :from-end t) target))))

(deftest test-split ()
  "Tests `split' programs."
  (let ((target "foo@bar.com"))
    (check
      (equal (split target "@") '("foo" "bar.com"))
      (equal (split target ".") '("foo@bar" "com"))
      (equal (split target "") '("f" "o" "o" "@" "b" "a" "r" "." "c" "o" "m"))
      (equal (split target " ") (list target)))))

(deftest test-split-idx ()
  "Tests `split-idx' programs."
  (let ((target "foo@bar.com"))
    (check
      (string= (split-idx target "@" 0) "foo")
      (string= (split-idx target "." 1) "com")
      (string= (split-idx target "" (1- (length target))) "m")
      (string= (split-idx target " " 0) target))))

(deftest test-concat ()
  "Tests `concat' programs."
  (check
    (string= (concat (literal "a") (literal "b")) "ab")
    (string= (concat (literal "a") (concat (literal "b") (literal "c"))) "abc")
    (string= (concat (concat (literal "m") (literal "a"))
		     (concat (literal "g")
			     (concat (literal "i") (literal "c"))))
	     "magic")))

(deftest test-grammar ()
  "Tests grammar programs."
  (combine-results
    (test-literal)
    (test-sub-str)
    (test-split)
    (test-split-idx)
    (test-concat)))
