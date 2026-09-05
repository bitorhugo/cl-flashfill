(in-package :cl-flashfill)

(defun is (fn &rest args)
  (let ((success-p (apply fn args)))
    (cond (success-p
	   (format t "PASS~%")
	   (values t))
	  (t
	   (format t "FAIL: fn~a~%" fn)))))

(defun run-grammar-tests ()
  (is #'string= "John" (sub-str "John Smith" 0 4))

  (is #'string= "John" (sub-str "John Smith" 6 10 :from-end t))

  (is #'string= "J. Smith" (concat (sub-str "John Smith" 0 1)
				   (literal ".")
				   (literal " ") ; we could use sub-str starting from 4
				   (sub-str "John Smith" 5 10)))

  (is #'string= "J. Doe" (concat (sub-str "Jane Doe" 0 1)
				 (literal ".")
				 (literal " ")
				 (sub-str "Jane Doe" 5 8)))

  (is #'equal (list "John" "Smith") (split "John Smith" " "))

  (is #'string=  "John" (split-idx "John Smith" " " 0)))

(defun run-eval-tests ()
  (is #'string=  "Smith" (eval-prog '(split-idx " " 1) "John Smith"))

  (is #'string= "John" (eval-prog '(sub-str 0 4) "John Smith"))

  (is #'string= "!" (eval-prog '(literal "!")
			       "John Smith"))
  (is #'string= "J. Smith" (eval-prog '(concat (sub-str 0 1)
					(literal ".")
					(literal " ")
					(sub-str 5 10))
				      "John Smith")))

(defun run-search-tests ()
  (is #'<
      (length (prune-equivalent (all-depth-1-programs '(("ab" . "")))
				'(("ab" . ""))))
      (length (all-depth-1-programs '(("ab" . "")))))
  (is #'equal
      (filter-correct (list '(literal "w")
			    '(concat (sub-str 0 1) (literal ".")))
		      '(("Jane Doe" . "J.")))
      '((concat (sub-str 0 1) (literal ".")))))

(defun run-ranking-tests ()
  ;; program size
  ;;
  (is #'= (program-size '(literal "J")) 1)
  (is #'= (program-size '(sub-str "Jane" 0 0)) 1)
  (is #'= (program-size '(split "Doe" "o")) 1)
  (is #'= (program-size '(split-idx "Jane Doe" " " 0)) 1)
  (is #'= (program-size '(concat (literal "J") (literal "."))) 3)
  ;; rank
  ;;
  (is #'equal
      (nrank '((literal "l")) :by #'program-size)
      '((literal "l")))
  (is #'equal
      (nrank '((concat (literal "j") (literal ".")) (literal "l")) :by #'program-size)
      '((literal "l") (concat (literal "j") (literal "."))))
  ;; smallest-program
  ;;
  (is #'equal
      (smallest-program '((concat (literal "j") (literal ".")) (literal "l")))
      '(literal "l"))
  (is #'equal
      (smallest-program '((concat (literal "j") (literal "."))
			  (concat (concat (literal "d") (literal "f")) (literal "."))))
      '(concat (literal "j") (literal "."))))

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
  (is #'= (length (all-depth-2-programs '(("jd" . "j")))) 11)
  (is #'null
      (remove-if (lambda (p)
		   (member p (all-depth-2-programs '(("jd" . "j")))
			   :test #'equal))
		 '((literal "d") (literal "j") (split-idx " " 0)
		   (concat (literal "d") (literal "d")) (concat (literal "d") (literal "j"))
		   (concat (literal "d") (split-idx " " 0)) (concat (literal "j") (literal "j"))
		   (concat (literal "j") (split-idx " " 0))
		   (concat (split-idx " " 0) (literal "d"))
		   (concat (split-idx " " 0) (literal "j"))
		   (concat (split-idx " " 0) (split-idx " " 0)))))

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

(defun run-tests ()
  (run-grammar-tests)
  (run-eval-tests)
  (run-search-tests)
  (run-ranking-tests)
  (run-enumeration-tests)
  (run-synthesis-tests))
