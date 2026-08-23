(in-package :cl-flashfill)

;; Grammar

(defun sub-str (s start &optional end)
  (subseq s start end))

(defun literal (c) c)

(defun concat (&rest parts)
  (apply #'concatenate 'string parts))

;; Note:
;; The above are called grammar compositions, and together they form a grammar (DSL).
;; Every program written is some compostion of these, nothing else.
;; This restriction is what makes search over programs tracktable (easy to follow) later.

(defun eval-prog (form s)
  (let ((op (first form)))
    (cond ((eq 'sub-str op)
	   (apply op s (rest form)))
	  ((eq 'literal op)
	   (funcall op (second form)))
	  ((eq 'concat op)
	   (apply op (mapcar (lambda (sub-form) (eval-prog sub-form s))
			     (rest form))))))) ; <-- this is called structural recursion over a tree
					; a function calling itself on each child

(defun all-sub-str-programs (n)
  (let ((result (list)))
    (dotimes (i (1+ n) result)
      (dotimes (j (- n i))
	(push `(sub-str ,i ,(- n j))
	      result)))))

(defun all-literal-programs (c)
  (unless (zerop (length c))
    (cons `(literal ,(sub-str c 0 1))
	  (all-literal-programs (sub-str c 1)))))

(defun all-concat-programs (p)
  (let ((result (list)))
    (dotimes (i (length p) result)
      (dotimes (j (length p))
	(push `(concat ,(nth i p) ,(nth j p))
	      result)))))

(defun prune-equivalent (list-of-programs s)
  (let ((output->programs (make-hash-table :test 'equal)))
    (dolist (prog list-of-programs)
      (let ((output (eval-prog prog s)))
	(setf (gethash output output->programs)
	      prog)))
    (hash-table-values output->programs)))

(defun concat-and-prune (p s)
  (let ((output->programs (make-hash-table :test 'equal)))
    (dotimes (i (length p))
      (dotimes (j (length p))
	(let* ((program `(concat ,(nth i p) ,(nth j p)))
	       (output (eval-prog program s)))
	  (setf (gethash output output->programs)
		program))))
    (hash-table-values output->programs)))

(defun all-depth-1-programs (s out)
  (append (all-sub-str-programs (length s))
	  (all-literal-programs s)
	  (all-literal-programs out)))

(defun all-depth-2-programs (s out)
  (let ((d1p (all-depth-1-programs s out)))
    (prune-equivalent (append d1p (all-concat-programs d1p))
		      s)))

(defun all-depth-3-programs (s out)
  (let ((d2p (all-depth-2-programs s out)))
    (append d2p (concat-and-prune d2p s))))

(defun %filter-correct (list-of-programs input expected-output)
  (remove-if-not (lambda (program) (equal (eval-prog program input)
					  expected-output))
		 list-of-programs))

(defun filter-correct (list-of-programs input-output-pairs)
  (remove-if-not (lambda (program)
		   (some (lambda (pair)
			    (equal (eval-prog program (car pair))
				   (cdr pair)))
			  input-output-pairs))
		 list-of-programs))

(defun program-size (program)
  (cond ((or (eql (first program) 'literal)
	     (eql (first program) 'sub-str))
	 1)
	((eql (first program) 'concat)
	 (+ 1
	    (program-size (second program))
	    (program-size (third program))))
	(t 0)))

(defun smallest-program (programs)
  (let ((sorted (sort (mapcar (lambda (program)
				(cons program (program-size program)))
			      programs)
		      #'< :key #'cdr)))
    (car (first sorted))))


;; tests

(defun run-tests ()

  (let ((name "John Smith"))
    ;; John
    (format t "~a~%" (sub-str "John Smith" 0 4))

    ;; J. Smith -> J dot space Smith
    (format t "~a~%" (concat (sub-str "John Smith" 0 1)
			     (literal ".")
			     (literal " ") ; we could use sub-str starting from 4 as well
			     (sub-str "John Smith" 5))))

  (let ((name "Jane Doe"))
    (format t "~a~%" (concat (sub-str name 0 1)
			     (literal ".")
			     (literal " ")
			     (sub-str name 5))))

  (format t "~a~%" (eval-prog '(sub-str 0 4) "John Smith"))

  (format t "~a~%" (eval-prog '(literal "!") "John Smith"))

  (format t "~a~%" (eval-prog '(concat (sub-str 0 1) (literal ".") (literal " ") (sub-str 5))
			      "John Smith"))

  (format t "~a~%" (all-sub-str-programs 4))

  (print (all-literal-programs "ab"))

  (print (all-depth-1-programs "ab" ""))

  (print (%filter-correct (all-depth-1-programs "ab" "") "ab" "a"))
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
