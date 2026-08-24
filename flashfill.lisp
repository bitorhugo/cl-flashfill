(in-package :cl-flashfill)

;; Grammar

(defun sub-str (s start &optional end)
  "Returns a sub-string of S between START and optionally END;
Accepts negative values for START or END."
  (let ((len (length s)))
    (subseq s
	    (if (minusp start)
		(+ len start)
		start)
	    (when end
	      (if (minusp end)
		  (+ len end)
		  end)))))

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
	      (split (sub-str s (1+ delim-idx)) delim))
	(list s))))

(defun split-idx (s delim idx)
  "Wrapper for SPLIT that returns the NTH object."
  (nth idx (split s delim)))

;; Note:
;; The above are called grammar compositions, and together they form a grammar (DSL).
;; Every program written is some compostion of these, nothing else.
;; This restriction is what makes search over programs tracktable (easy to follow) later.

(defun eval-prog (form s)
  "Evaluates FORM with S and returrns its value."
  (let ((op (first form)))
    (handler-bind
	((error (lambda (c)	      ; MAYBE: log programs that error
		  (declare (ignore c))
		  (invoke-restart 'return-nil))))
      (restart-case
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
				   (rest form)))))
	(return-nil () :report "Return nil" (values))))))

(defun all-sub-str-programs (n)
  "Returns all combinations of SUB-STR programs."
  (let ((result (list)))
    (do ((start (- n) (1+ start)))
	((>= start n) result)
      (do ((end (- n) (1+ end)))
	  ((= end n))
	(unless (>= (mod start n)
		    (mod end n)) ; empty string and out-of-bounds indexes
	  (push `(sub-str ,start ,end) result)))
      (push `(sub-str ,start) result))))

(defun all-literal-programs (l)
  "Returns all combinations of LITERAL programs."
  (unless (zerop (length l))
    (cons `(literal ,(sub-str l 0 1))
	  (all-literal-programs (sub-str l 1)))))

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

(defun prune-equivalent (list-of-programs inputs)
  "Filters LIST-OF-PROGRAMS using observational-equivalence on INPUTS."
  (let ((output->programs (make-hash-table :test 'equal)))
    (dolist (prog list-of-programs)
      (setf (gethash (mapcar (curry #'eval-prog prog) inputs)
		     output->programs)
	    prog))
    (hash-table-values output->programs)))

(defun concat-and-prune (p s target)
  (let ((output->programs (make-hash-table :test 'equal)))
    (dotimes (i (length p))
      (dotimes (j (length p))
	(let* ((program `(concat ,(nth i p) ,(nth j p)))
	       (output (eval-prog program s)))
	  (when (and output
		     (relevant-p output target))
	    (setf (gethash output output->programs)
		  program)))))
    (hash-table-values output->programs)))

(defun relevant-p (p-out target-out)
  (search p-out target-out :test #'string=))

(defun all-depth-1-programs (s out)
  (nconc (all-sub-str-programs (length s))
	 (all-literal-programs s)
	 (all-literal-programs out)
	 (all-split-programs s " ")))

(defun all-depth-2-programs (s out)
  (let ((d1p (all-depth-1-programs (list s) out)))
    (prune-equivalent (nconc d1p (all-concat-programs d1p))
		      s)))

(defun all-depth-3-programs (s out)
  (let ((d2p (all-depth-2-programs s out)))
    (nconc d2p (concat-and-prune d2p s out))))

(defun all-depth-n-programs (s out n)
  (do ((i 3 (1+ i))
       (dxp (all-depth-2-programs s out)
	    (nconc dxp (concat-and-prune dxp s out))))
      ((> i n) dxp)))

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

(defun synthesize (examples &optional (rounds 5))
  (let* ((seed (car (first examples)))
	 (seed-output-example (cdr (first examples)))
	 (search-space (all-depth-n-programs seed seed-output-example rounds)))
    (filter-correct search-space examples)))
