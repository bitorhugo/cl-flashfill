;;;; SPDX-License-Identifier: MIT


(defun run-enumeration-tests ()
  (is #'equal
      (all-literal-programs "Jane")
      '((literal "e") (literal "n") (literal "a") (literal "J")))

  (is #'= (length (all-sub-str-programs 10)) (* 10 11))
  (is #'equal
      (all-split-programs "a b c" " ")
      '((split-idx " " 2) (split-idx " " 1) (split-idx " " 0)))
  (is #'= 4 (length (all-concat-programs '((literal "a")
					   (literal "b")))))

  (is #'null
      (remove-if (lambda (p)
		   (member p (all-depth-1-programs '(("j" . "jd")))
			   :test #'equal))
		 '((literal "j") (literal "d") (split-idx " " 0)
		   (sub-str 0 1) (sub-str 0 1 :from-end t)
		   (sub-str 0 2) (sub-str 0 2 :from-end t)
		   (sub-str 1 2) (sub-str 1 2 :from-end t))))

  ;; all-sub-str-programs generates normal and :from-end variants that
  ;; coincide whenever start+end = (length "jd"), so several concats
  ;; tie on the same output at the same size; which one survives depends
  ;; on nrank's (unstable) sort, so we assert the actual verified
  ;; survivor set rather than a specific representative per tie
  ;;
  (is #'= (length (all-depth-2-programs '(("jd" . "j")))) 3)
  (is #'null
      (remove-if (lambda (p)
		   (member p (all-depth-2-programs '(("jd" . "j")))
			   :test #'equal))
		 '((literal "d") (literal "j") (split-idx " " 0))))

  (is #'= (length (concat-and-prune '((literal "a") (literal "b")) '("x"))) 4)
  (is #'null
      (remove-if (lambda (p)
		   (member p (concat-and-prune '((literal "a") (literal "b")) '("x"))
			   :test #'equal))
		 '((concat (literal "a") (literal "a")) (concat (literal "a") (literal "b"))
		   (concat (literal "b") (literal "a")) (concat (literal "b") (literal "b")))))

  ;; a and (sub-str 0 1) behave identically on input "a", so every
  ;; (a . b) pairing collapses to the same signature and only the
  ;; first-seen concat survives
  ;;
  (is #'equal
      (concat-and-prune '((literal "a") (sub-str 0 1)) '("a"))
      '((concat (literal "a") (literal "a")))))

(defun run-synthesis-tests ()
  (is #'eval-prog
      (synthesize '(("Jane" . "J.")
		    ("Hugo" . "H.")))
      "Jane"))
