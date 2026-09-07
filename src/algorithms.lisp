;;;; SPDX-License-Identifier: MIT

(in-package #:flashfill)

;;;; pruning

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

(defun relevant-p (signature outputs)
  "Checks if SIGNATURE is relevant against OUTPUTS."
  (every (lambda (signature output)
	   (search signature output :test #'string=))
	 signature outputs))

(defun filter-correct (programs examples)
  "Returns a subset of PROGRAMS that evaluate to EXAMPLES."
  (loop for program in programs
	when (every (lambda (ex)
		      (equal (eval-prog program (car ex))
			     (cdr ex)))
		    examples)
	  collect program))

(defun program-size (program)
  "Returns the size of PROGRAMS. This is the cost function."
  (let ((p (first program)))
    (cond ((eql p 'concat)
	   (+ 4
	      (program-size (second program))
	      (program-size (third program))))
	  ((eql p 'sub-str) 3)
	  ((eql p 'split-idx) 2)
	  (t 1))))

(defun smallest-program (programs)
  "Follows Occam's razor criteria, where we prefer the smallest possible set."
  (first (nrank programs :by #'program-size)))


;;;; synthesize

(defun synthesize (examples &key (depth 3))
  "Synthesizes programs from EXAMPLES."
  (let ((search-space (all-programs depth examples)))
    (smallest-program (filter-correct search-space examples))))
