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


(deftest test-algorithms ()
  "Tests algorithms programs."
  (combine-results
    (test-prune-equivalent)
    (test-filter-correct)))
