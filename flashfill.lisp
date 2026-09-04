(in-package :cl-flashfill)


;; Grammar
;;

(defun sub-str (s start end &key from-end)
  "Returns a sub-string of S between START and END;"
  (if from-end
      (let ((len (length s)))
	(subseq s
		(cond ((zerop end) 0)
		      (t (- len end)))
		(- len start)))
      (subseq s start end)))

(defun literal (l)
  "Returns the literal L."
  l)

(defun concat (&rest parts)
  "Returns the concatenation of PARTS."
  (apply #'concatenate 'string parts))

(defun split (s delim)
  "Returns a LIST of sub-strings of S delimited by DELIM."
  (let ((delim-idx (position delim s :test #'string=)))
    (if delim-idx
	(cons (sub-str s 0 delim-idx)
	      (split (sub-str s (1+ delim-idx) (length s)) delim))
	(list s))))

(defun split-idx (s delim idx)
  "Wrapper for SPLIT that returns the NTH object."
  (nth idx (split s delim)))

;; Note:
;; The above are called grammar compositions, and together they form a grammar (DSL).
;; Every program written is some compostion of these, nothing else.
;; This restriction is what makes search over programs tracktable (easy to follow) later.

(defun eval-prog (form s)
  "Evaluates FORM with S and returns its value."
  (handler-bind
      ((error (lambda (c)	      ; MAYBE: log programs that error
		(declare (ignore c))
		(invoke-restart 'return-nil))))
    (restart-case
	(let ((op (first form)))
	  (cond ((eq 'sub-str op)
		 (apply op s (rest form)))
		((eq 'split-idx op)
		 (apply op s (rest form)))
		((eq 'literal op)
		 (funcall op (second form)))
		((eq 'concat op)
		 ;; structural recursion over a tree
		 ;; a function calling itself on each child
		 (apply op (mapcar (lambda (sub-form) (eval-prog sub-form s))
				   (rest form))))))
      (return-nil () :report "Return nil" (values)))))

(defun all-sub-str-programs (n)
  "Returns all combinations of SUB-STR programs."
  (let ((result (list)))
    (do ((start 0 (1+ start)))
	((> start (1- n)) result)
      (do ((end (1+ start) (1+ end)))
	  ((> end n))
	(push `(sub-str ,start ,end)
	      result)
	(push `(sub-str ,start ,end :from-end t)
	      result)))))

(defun %all-literal-programs (s &optional acc)
  "Helper function of ALL-LITERAL-PROGRAMS."
  (let ((len (length s)))
    (if (zerop len)
	acc
	(%all-literal-programs (sub-str s 1 len)
			       (cons `(literal ,(sub-str s 0 1))
				     acc)))))

(defun all-literal-programs (s)
  "Returns all combinations of LITERAL programs of S."
  (%all-literal-programs s))

(defun all-split-programs (s delim)
  "Returns all combinations of SPLIT-IDX programs."
  (let ((result (list)))
    (dotimes (i (length (split s delim)) result)
      (push `(split-idx ,delim ,i)
	    result))))

(defun all-concat-programs (p)
  "Returns all combinations of CONCAT programs."
  (let ((result (list)))
    (dotimes (i (length p) result)
      (dotimes (j (length p))
	(push `(concat ,(nth i p) ,(nth j p))
	      result)))))

(defun prune-equivalent (programs input-examples)
  "Filters PROGRAMS using observational equivalence across EXAMPLES."
  (loop with signature->program = (make-hash-table :test 'equal)
        for program in programs
	;; evaluate program against each example
	;; this becomes the signature vector
        for signature = (mapcar (curry #'eval-prog program) input-examples)
	unless (some #'null signature)
	  ;; given that programs are ranked
	  ;; we only save the first (best by rank's definition)
	  do (unless (gethash signature signature->program)
	       (setf (gethash signature signature->program)
		     program))
        finally
	   (return (hash-table-values signature->program))))

(defun concat-and-prune (p input-examples)
  (loop with sig->program = (make-hash-table :test 'equal)
        for a in p
        do (loop for b in p
                 for program = `(concat ,a ,b)
                 for signature = (mapcar (curry #'eval-prog program)
					 input-examples)
		 unless (some #'null signature)
		   do (unless (gethash signature sig->program)
			(setf (gethash signature sig->program)
			      program)))
        finally
	   (return (hash-table-values sig->program))))

(defun all-depth-1-programs (examples)
  (loop with string-len = (reduce #'max examples
				  :key (lambda (x)
					 (max (length (car x))
					      (length (cdr x))))
				  :initial-value 0)
	for (input . output) in examples
	nconc (nconc (all-literal-programs input)
		     (all-literal-programs output)
		     (all-split-programs input " ")
		     (all-split-programs output " "))
	  into programs
	finally
	   (return (nconc programs
			  (all-sub-str-programs string-len)))))

(defun all-depth-2-programs (examples)
  (let* ((d1 (all-depth-1-programs examples))
	 (ranked (nrank (nconc (all-concat-programs d1) d1)
			:by #'program-size)))
    (prune-equivalent ranked
		      (mapcar #'car examples))))

#+n(defun all-depth-3-programs (examples)
     (let ((d2 (all-depth-2-programs examples)))
       (nconc d2 (concat-and-prune d2 (mapcar #'car examples)))))

#+n(defun all-depth-4-programs (examples)
     (let ((d3 (all-depth-3-programs examples)))
       (nconc d3 (concat-and-prune d3 (mapcar #'car examples)))))

(defun all-depth-n-programs (n examples)
  (let ((input-examples (mapcar #'car examples)))
    (loop with dn = (all-depth-2-programs examples)
	  repeat (- n 2)
	  do (setf dn (nrank (nconc dn (concat-and-prune dn input-examples))
			     :by #'program-size))
	  finally
	     (return dn))))

(defun filter-correct (programs examples)
  (loop for program in programs
	when (every (lambda (ex)
		      (equal (eval-prog program (car ex))
			     (cdr ex)))
		    examples)
	  collect program))

(defun program-size (program)
  (cond ((eql (first program) 'concat)
	 (+ 1
	    (program-size (second program))
	    (program-size (third program))))
	(t 1)))

(defun nrank (programs &key (by #'identity))
  (sort programs #'< :key by))

(defun smallest-program (programs)
  "Follows Occam's razor criteria, where we prefer the smallest possible set."
  (first (nrank programs :by #'program-size)))

(defun synthesize (examples &key (depth 3))
  (let ((search-space (all-depth-n-programs depth examples)))
    (smallest-program (filter-correct search-space examples))))
