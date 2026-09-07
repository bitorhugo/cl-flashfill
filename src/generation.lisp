;;;; SPDX-License-Identifier: MIT

(in-package #:flashfill)

;;;; literal generation

(defun all-literal-programs (s)
  "Returns all combinations of LITERAL programs of S."
  (labels ((%all-literal-programs (s &optional acc)
	     (let ((len (length s)))
	       (if (zerop len)
		   acc
		   (%all-literal-programs (sub-str s 1 len)
					  (cons `(literal ,(sub-str s 0 1))
						acc))))))
    (%all-literal-programs s)))

;;;; sub-str generation

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

;;;; split generation

(defun all-split-programs (s)
  (loop with literals = (all-literal-programs s)
	for l in literals
	nconc (loop with split = (split s (eval-prog l s))
		    for i from 0 to (1- (length split))
		    collect `(split-idx ,l ,i))))

;;;; concat generation

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
		 (setf j-gen
		       (%concat-j-generator i programs))
		 (funcall j-gen)))
	      (t
	       next))))))

(defun concat-generator (programs)
  (let ((p (make-array (length programs) ; use arrays for O(1) indexing
		       :initial-contents programs)))
    (%concat-generator p)))

;;;; all programs generation

(defun seed-programs (examples)
  "Generates seed programs based on EXAMPLES."
  (let ((seen (make-hash-table :test 'equal :size 500))
	(max-len (reduce #'max examples :key (lambda (x)
					       (max (length (car x))
						    (length (cdr x))))
					:initial-value 0)))
    (flet ((add-all (programs)
	     (loop for p in programs
		   do (setf (gethash p seen) t))))
      (loop for (input . output) in examples
	    do (add-all (all-literal-programs input))
	       (add-all (all-literal-programs output))
	       (add-all (all-split-programs input))
	       (add-all (all-split-programs output)))
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
	    do (when (and (relevant-p signature outputs)
			  (or (null (gethash signature signature->program))
			      (< (program-size program)
				 (program-size (gethash signature signature->program)))))
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

(defun all-programs (n examples)
  "Returns all synthesized programs from EXAMPLES after N cycles."
  (let ((inputs (mapcar #'car examples))
	(outputs (mapcar #'cdr examples)))
    (loop with dn = (concat-extend (seed-programs examples) inputs outputs)
	  repeat (- n 2)
	  do (setf dn (concat-extend dn inputs outputs))
	  finally (return dn))))
