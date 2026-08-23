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
    (handler-case
	(restart-case
	    (cond ((eq 'sub-str op)
		   (apply op s (rest form)))
		  ((eq 'literal op)
		   (funcall op (second form)))
		  ((eq 'concat op)
		   (apply op (mapcar (lambda (sub-form) (eval-prog sub-form s))
				     (rest form))))) ; <-- this is called structural recursion over a tree
					; a function calling itself on each child
	  (return-nil () :report "Return nil" (values)))
      (error (c) ; MAYBE: log programs that error
	(declare (ignore c))
	(invoke-restart 'return-nil)))))


(defun all-sub-str-programs (n)
  (let ((result (list)))
    (dotimes (i (1+ n) (reverse result))
      (dotimes (j (- n i))
	(push `(sub-str ,i ,(- n j))
	      result))
      (push `(sub-str ,i) result))))

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
		   (every (lambda (pair)
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

(defun synthesize (examples)
  (let* ((seed (car (first examples)))
	(seed-output-example (cdr (first examples)))
	(search-space (all-depth-3-programs seed seed-output-example)))
    (filter-correct search-space examples)))
