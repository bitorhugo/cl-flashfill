;;;; SPDX-License-Identifier: MIT

(in-package #:flashfill)

(define-condition progress-report-condition ()
  ((clause-id :initarg :clause-id :reader clause-id)
   (args :initarg :args :reader progress-report-condition-args))
  (:documentation "Represents the reaching of a milestone."))

(defun report-progress (clause-id &rest args)
  "Signals the milestone."
  (signal 'progress-report-condition :clause-id clause-id
				     :args args))

(defun call-tracking-progress (thunk handler)
  (handler-bind ((progress-report-condition handler))
    (funcall thunk)))

(defmacro tracking-progress (form &body clauses)
  "Creates an environment where CLAUSES are signaled via REPORT-PROGRESS."
  (with-gensyms (condition)
    `(call-tracking-progress
      #'(lambda () ,form)
      #'(lambda (,condition)
	  (case (clause-id ,condition)
	    ,@(loop for (id args . forms) in clauses
		    collect `(,id (,@(if args
					 `(destructuring-bind ,args
					      (progress-report-condition-args ,condition))
					 '(progn))
				   ,@forms))))))))
