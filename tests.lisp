(in-package :cl-flashfill)

(defun is (fn &rest args)
  (let ((success-p (apply fn args)))
    (if success-p
	(format t "PASS~%")
	(format t "FAIL: ~a~%" args))))

(defun run-tests ()

  (let ((name "John Smith"))

    ;; John
    (is #'string= "John" (sub-str "John Smith" 0 4))
    (is #'string= "John" (sub-str "John Smith" -10 4))

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

  ;; TODO: clean
  ;;
  (format t "after prunning: ~a programs~%"
	  (length (prune-equivalent (all-depth-1-programs "ab" "") "ab")))
  (format t "depth 2 pruned: ~a programs~%" (length (all-depth-2-programs "John Smith" "")))
  (format t "depth 3 pruned: ~a programs~%" (length (all-depth-3-programs "Jane" "")))

  (print (filter-correct (all-depth-3-programs "Jane Doe" "J. Doe") '(("Jane Doe" . "J. Doe"))))
  (print (filter-correct (all-depth-3-programs "V Hugo" "V. Hugo") '(("V Hugo" . "V. Hugo"))))

  (print (program-size '(concat (literal "J") (concat (literal ".") (sub-str 4 8)))))

  (print (smallest-program (filter-correct (all-depth-3-programs "Jane Doe" "J. Doe") '(("Jane Doe" . "J. Doe"))))))
