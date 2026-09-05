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

(defun %sub-str-end-generator (n start)
  (let* ((str-len n)
	 (end (1+ start))
	 (from-end-p nil))
    (lambda ()
      (unless (> end str-len)
	(cond (from-end-p
	       (prog1
		   `(sub-str ,start ,end :from-end t)
		 (incf end)
		 (setf from-end-p nil)))
	      (t
	       (prog1
		   `(sub-str ,start ,end)
		 (setf from-end-p t))))))))

(defun %sub-str-start-generator (n)
  (let* ((str-len n)
	 (start 0)
	 (end-gen (%sub-str-end-generator str-len start)))
    (lambda ()
      (unless (> start (1- str-len))
	(let ((next (funcall end-gen)))
	  (cond ((null next)
		 (incf start)
		 (setf end-gen
		       (%sub-str-end-generator str-len start))
		 (funcall end-gen))
		(t
		 next)))))))

(defun sub-str-generator (n)
  (%sub-str-start-generator n))

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


(defun %concat-j-generator (i programs)
  (let ((n (1- (length programs)))
	(j 0))
    (lambda ()
      (unless (> j n)
	(prog1
	    `(concat ,(aref programs i) ,(aref programs j))
	  (incf j))))))

(defun %concat-generator (programs)
  (let* ((n (1- (length programs)))
	 (i 0)
	 (j-gen (%concat-j-generator i programs)))
    (lambda ()
      (let ((next (funcall j-gen)))
	(cond ((null next)
	       (when (< i n)
		 (incf i)
		 (setf j-gen (%concat-j-generator i programs))
		 (funcall j-gen)))
	      (t
	       next))))))

(defun concat-generator (programs)
  ;; use arrays for O(1) indexing
  ;;
  (let ((p (make-array (length programs) :initial-contents programs)))
    (%concat-generator p)))

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
  (let ((seen (make-hash-table :test 'equal :size 500))
	(max-len (reduce #'max examples :key (lambda (x)
					       (max (length (car x))
						    (length (cdr x))))
					:initial-value 0)))
    (flet ((add-all (programs)
	     (dolist (p programs)
	       (setf (gethash p seen) t))))
      (loop for (input . output) in examples
	    do (add-all (all-literal-programs input))
	       (add-all (all-literal-programs output))
	       (add-all (all-split-programs input " "))
	       (add-all (all-split-programs output " ")))
      ;; we need to only take into consideration the longest
      ;;
      (loop with gen = (sub-str-generator max-len)
	    for next = (funcall gen)
	    while next
	    do (setf (gethash next seen) t))
      ;; return unique keys
      ;;
      (loop for p being the hash-keys of seen
	    collect p))))

(defun relevant-p (signature outputs)
  (every (lambda (signature output)
	   (search signature output :test #'string=))
	 signature outputs))

(defun concat-extend (programs inputs outputs)
  "Extends PROGRAMS with CONCAT combinations of themselves, pruned by
observational equivalence against INPUT-EXAMPLES. A PROGRAMS member
or a generated CONCAT only replaces the current signature holder when
it is strictly smaller, so smaller programs always win a tie."
  (let ((signature->program (make-hash-table :test 'equal :size 100000)))
    ;; seed signature->program with PROGRAMS, keeping the smallest
    ;; per signature; no sort needed since ties are broken by an
    ;; explicit size comparison rather than visit order
    ;;
    (loop for program in programs
          for signature = (mapcar (curry #'eval-prog program) inputs)
	  unless (some #'null signature)
	    do (when (or (null (gethash signature signature->program))
			 (< (program-size program)
			    (program-size (gethash signature signature->program))))
		 (setf (gethash signature signature->program) program)))
    ;; lazy generate concat programs and rank them by program size
    ;;
    (loop with gen = (concat-generator programs)
	  for program = (funcall gen)
	  while program
	  for signature = (mapcar (curry #'eval-prog program) inputs)
	  unless (some #'null signature)
	    do (when (and (relevant-p signature outputs)
			  (or (null (gethash signature signature->program))
			      (< (program-size program)
				 (program-size (gethash signature signature->program)))))
		 (setf (gethash signature signature->program)
		       program)))
    ;; finally return the programs
    ;;
    (loop for k being the hash-values of signature->program
	  collect k)))

(defun all-depth-2-programs (examples)
  (let ((inputs (mapcar #'car examples))
	(outputs (mapcar #'cdr examples)))
    (concat-extend (all-depth-1-programs examples)
		   inputs
		   outputs)))

(defun all-depth-n-programs (n examples)
  (let ((inputs (mapcar #'car examples))
	(outputs (mapcar #'cdr examples)))
    (loop with dn = (all-depth-2-programs examples)
	  repeat (- n 2)
	  do (setf dn (concat-extend dn inputs outputs))
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
  (let ((p (first program)))
    (cond ((eql p 'concat)
	   (+ 4
	      (program-size (second program))
	      (program-size (third program))))
	  ((eql p 'sub-str) 3)
	  ((eql p 'split-idx) 2)
	  (t 1))))

(defun nrank (programs &key (by #'identity))
  (sort programs #'< :key by))

(defun smallest-program (programs)
  "Follows Occam's razor criteria, where we prefer the smallest possible set."
  (first (nrank programs :by #'program-size)))

(defun synthesize (examples &key (depth 3))
  (let ((search-space (all-depth-n-programs depth examples)))
    (smallest-program (filter-correct search-space examples))))
