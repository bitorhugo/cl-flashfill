;;;; SPDX-License-Identifier: MIT

(in-package #:flashfill)

(deftest test-all-literal-programs ()
  "Tests `all-literal-programs' programs."
  (check
    (equal (all-literal-programs "Jane")
	   '((literal "e") (literal "n") (literal "a") (literal "J")))))

(deftest test-sub-str-generator ()
  "Tests `sub-str-generator'."
  (let ((gen (sub-str-generator 1)))
    (check
      (equal (funcall gen) '(sub-str 0 1))
      (equal (funcall gen) '(sub-str 0 1 :from-end t))
      (null (funcall gen)))))

(deftest test-all-split-programs ()
  "Tests `all-split-programs'."
  (check
    (equal (all-split-programs "foo")
	   '((split-idx (literal "o") 0) (split-idx (literal "o") 1)
	     (split-idx (literal "o") 2) (split-idx (literal "o") 0)
	     (split-idx (literal "o") 1) (split-idx (literal "o") 2)
	     (split-idx (literal "f") 0) (split-idx (literal "f") 1)))))

(deftest test-concat-generator ()
  "Tests `concat-generator'."
  (let ((gen (concat-generator '((literal "a") (literal "b")))))
    (check
      (equal (funcall gen) '(concat (literal "a") (literal "a")))
      (equal (funcall gen) '(concat (literal "a") (literal "b")))
      (equal (funcall gen) '(concat (literal "b") (literal "a")))
      (equal (funcall gen) '(concat (literal "b") (literal "b"))))))

(deftest test-generation ()
  "Tests `generation' programs."
  (combine-results
    (test-all-literal-programs)
    (test-sub-str-generator)
    (test-all-split-programs)
    (test-concat-generator)))
