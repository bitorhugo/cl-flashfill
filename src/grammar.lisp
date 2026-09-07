;;;; SPDX-License-Identifier: MIT

(in-package #:flashfill)

;;;; expr ::= literal
;;;;          | sub-str int int bool (always applied to raw input)
;;;;          | split-idx expr int   (delimiter is an expr, e.g. literal)
;;;;          | concat expr expr
;;;;

(defun literal (l)
  "Returns the literal l."
  l)

(defun sub-str (s start end &key from-end)
  "Returns a substring of S between START and END;"
  (if from-end
      (let ((len (length s)))
	(subseq s
		(- len end)
		(- len start)))
      (subseq s start end)))

(defun split (expr delim)
  "Returns a LIST of sub-strings of EXPR delimited by DELIM."
  (labels ((recur (expr delim)
	     (let ((delim-idx (position delim expr :test #'string=)))
	       (if delim-idx
		   (cons (sub-str expr 0 delim-idx)
			 (recur (sub-str expr (1+ delim-idx)
					 (length expr))
				delim))
		   (list expr)))))
    (if (string= delim "")
	(mapcar #'string (coerce expr 'list))
	(recur expr delim))))

(defun split-idx (expr delim idx)
  "Wrapper for SPLIT that returns the NTH object."
  (nth idx (split expr delim)))

(defun concat (expr1 expr2)
  "Returns the concatenation of EXPR1 with EXPR2."
  (concatenate 'string expr1 expr2))

(defun eval-prog (expr s)
  "Evaluates EXPR with S and returns its value."
  (handler-bind
      ((error (lambda (c)
		(declare (ignore c))
		;; TODO: Log erroneous programs
		;;
		(invoke-restart 'return-nil))))
    (restart-case
	(let ((op (first expr)))
	  (cond ((eq 'literal op)
		 (second expr))
		((eq 'sub-str op)
		 (apply op s (rest expr)))
		((eq 'split-idx op)
		 (funcall op
			  s
			  (eval-prog (second expr) s)
			  (third expr)))
		((eq 'concat op)
		 (funcall op
			  (eval-prog (second expr) s)
			  (eval-prog (third expr) s)))))
      (return-nil ()
	:report "Return NIL" (values)))))
