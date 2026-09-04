(in-package :cl-flashfill)

(defun is (fn &rest args)
  (let ((success-p (apply fn args)))
    (cond (success-p
	   (format t "PASS~%")
	   (values t))
	  (t
	   (format t "FAIL: ~a~%" args)))))

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
      (rank '((literal "l")) :by #'program-size)
      '((literal "l")))
  (is #'equal
      (rank '((concat (literal "j") (literal ".")) (literal "l")) :by #'program-size)
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
					   (literal "b"))))))

(defun run-tests ()
  (run-grammar-tests)
  (run-eval-tests)
  (run-search-tests)
  (run-ranking-tests)
  (run-enumeration-tests))
