(in-package :cl-flashfill)

(defun is (fn &rest args)
  (apply fn args))

(defun run-tests ()

  (let ((name "John Smith"))

    ;; John
    (is #'string= "John" (sub-str "John Smith" 0 4))

    ;; J. Smith -> J dot space Smith
    (is #'string= "J. Smith" (concat (sub-str "John Smith" 0 1)
				     (literal ".")
				     (literal " ") ; we could use sub-str starting from 4 as well
				     (sub-str "John Smith" 5))))
  
  (let ((name "Jane Doe"))
    (is #'string= "J. Doe" (concat (sub-str name 0 1)
				   (literal ".")
				   (literal " ")
				   (sub-str name 5))))

  (is #'string= "John" (eval-prog '(sub-str 0 4) "John Smith"))

  (is #'string= "!" (eval-prog '(literal "!")
			       "John Smith"))

  (is #'string= "J. Smith" (eval-prog '(concat (sub-str 0 1) (literal ".") (literal " ") (sub-str 5))
				      "John Smith"))

  (is #'equal
      '((sub-str 3 4) (sub-str 2 3) (sub-str 2 4) (sub-str 1 2) (sub-str 1 3)
	(sub-str 1 4) (sub-str 0 1) (sub-str 0 2) (sub-str 0 3) (sub-str 0 4))
      (all-sub-str-programs 4))


  (is #'equal
      '((literal "a") (literal "b"))
      (all-literal-programs "ab"))

  (is #'equal
      (all-depth-1-programs "ab" "")
      '((sub-str 1 2) (sub-str 0 1) (sub-str 0 2) (literal "a") (literal "b")))

  (is #'equal
      (%filter-correct (all-depth-1-programs "ab" "") "ab" "a")
      '((sub-str 0 1) (literal "a")))
  ;; (mapcar (rcurry #'eval-prog "ab") '((SUB-STR 0 1) (LITERAL "a"))) = ("a" "a")

  (print (filter-correct (all-depth-1-programs "ab" "") '(("ab" . "a") ("ab" . "a"))))

  (print (filter-correct (all-depth-1-programs "ab" "") '(("ab" . "a") ("ab" . "b"))))

  (print (length (all-concat-programs (all-depth-1-programs "ab" ""))))

  (format t "depth 1: ~a programs~%" (length (all-depth-1-programs "John Smith" "")))
  (format t "depth 2: ~a programs~%" (length (all-depth-2-programs "John Smith" "")))

  (format t "after prunning: ~a programs~%"
	  (length (prune-equivalent (all-depth-1-programs "ab" "") "ab")))
  (format t "depth 2 pruned: ~a programs~%" (length (all-depth-2-programs "John Smith" "")))
  (format t "depth 3 pruned: ~a programs~%" (length (all-depth-3-programs "Jane" "")))

  (print (filter-correct (all-depth-3-programs "Jane Doe" "J. Doe") '(("Jane Doe" . "J. Doe"))))
  (print (filter-correct (all-depth-3-programs "V Hugo" "V. Hugo") '(("V Hugo" . "V. Hugo"))))

  (print (program-size '(concat (literal "J") (concat (literal ".") (sub-str 4 8)))))

  (print (smallest-program (filter-correct (all-depth-3-programs "Jane Doe" "J. Doe") '(("Jane Doe" . "J. Doe"))))))
