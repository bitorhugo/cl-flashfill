;;;; SPDX-License-Identifier: MIT

(in-package #:flashfill)

(deftest test-prune-equivalent ()
  "Tests `prune-equivalent' programs."
  (check
    (< (length (prune-equivalent (seed-programs '(("ab" . "")))
				 '(("ab" . ""))))
       (length (seed-programs '(("ab" . "")))))))

(deftest test-filter-correct ()
  "Tests `filter-correct' programs."
  (check
    (equal (filter-correct (list '(literal "w")
				 '(concat (sub-str 0 1) (literal ".")))
			   '(("Jane Doe" . "J.")))
	   '((concat (sub-str 0 1) (literal "."))))))

(deftest test-program-size ()
  "Tests `program-size' programs."
  (check
    (= (program-size '(literal "J")) 1)
    (= (program-size '(sub-str "Jane" 0 0)) 3)
    (= (program-size '(split-idx "Jane Doe" " " 0)) 2)
    (= (program-size '(concat (literal "J") (literal "."))) (+ 4 1 1))))

(deftest test-nrank ()
  "Tests `nrank' programs."
  (check
    (equal (nrank '((literal "l")) :by #'program-size)
	   '((literal "l")))
    (equal (nrank '((concat (literal "j") (literal ".")) (literal "l")) :by #'program-size)
	   '((literal "l") (concat (literal "j") (literal "."))))))

(deftest test-smallest-programs ()
  "Tests `smallest-programs' programs."
  (check
    (equal (smallest-program '((concat (literal "j") (literal ".")) (literal "l")))
	   '(literal "l"))
    (equal (smallest-program '((concat (literal "j") (literal "."))
			       (concat (concat (literal "d") (literal "f")) (literal "."))))
	   '(concat (literal "j") (literal ".")))))

(deftest test-algorithms ()
  "Tests algorithms programs."
  (combine-results
    (test-prune-equivalent)
    (test-filter-correct)
    (test-program-size)
    (test-nrank)
    (test-smallest-programs)))
