;; Utils

(defun curry (fn &rest args)
  #'(lambda (&rest more-args)
      (apply fn (append args more-args))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest more-args)
      (apply fn (append more-args args))))

(defun rac (list)
  (car (last list)))

;; Grammar

(defun sub-str (s start &optional end)
  (subseq s start end))

(defun literal (c) c)

(defun concat (&rest parts)
  (apply #'concatenate 'string parts))

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

(defun all-depth-1-programs (s)
  (append (all-sub-str-programs (length s))
	  (all-literal-programs s)))

(defun all-depth-2-programs (s)
  (let ((d1p (all-depth-1-programs s)))
    (append d1p (all-concat-programs d1p))))

(defun all-depth-3-programs (s)
  (let ((d2p (all-depth-2-programs s)))
    (append d2p (all-concat-programs d2p)))) ; at this point we have 18M programs.. Intractable

(defun %filter-correct (list-of-programs input expected-output)
  (remove-if-not (lambda (program) (equal (eval-prog program input)
					  expected-output))
		 list-of-programs))

(defun filter-correct (list-of-programs input-output-pairs)
  (remove-if-not (lambda (program)
		   (every (lambda (pair)
			    (equal (eval-prog program (car pair))
				   (cdr pair)))
			  input-output-pairs))
		 list-of-programs))

;; Note:
;; The above are called grammar compositions, and together they form a grammar (DSL).
;; Every program written is some compostion of these, nothing else.
;; This restriction is what makes search over programs tracktable (easy to follow) later.


;; tests

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

(print (all-depth-1-programs "ab"))

(print (%filter-correct (all-depth-1-programs "ab") "ab" "a"))
;; (mapcar (rcurry #'eval-prog "ab") '((SUB-STR 0 1) (LITERAL "a"))) = ("a" "a")

(print (filter-correct (all-depth-1-programs "ab") '(("ab" . "a") ("ab" . "a"))))

(print (filter-correct (all-depth-1-programs "ab") '(("ab" . "a") ("ab" . "b"))))

(print (length (all-concat-programs (all-depth-1-programs "ab"))))

(format t "depth 1: ~a programs~%" (length (all-depth-1-programs "John Smith")))
(format t "depth 2: ~a programs~%" (length (all-depth-2-programs "John Smith")))
