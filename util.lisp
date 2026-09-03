(in-package :cl-flashfill)

(defun curry (function &rest args)
  #'(lambda (&rest more-args)
      (apply function (append args more-args))))

(defun rcurry (function &rest args)
  #'(lambda (&rest more-args)
      (apply function (append more-args args))))

(defun rac (list)
  (car (last list)))

(defun hash-table->list (hash-table)
  (let ((result (list)))
    (maphash (lambda (k v) (push (cons k v) result)) hash-table)
    (values result)))

(defun map-hash-keys (function hash-table)
  (maphash (lambda (k v)
	     (declare (ignore v))
	     (funcall function k))
	   hash-table))

(defun map-hash-values (function hash-table)
  (maphash (lambda (k v)
	     (declare (ignore k))
	     (funcall function v))
	   hash-table))

(defun hash-table-keys (hash-table)
  (let ((result (list)))
    (map-hash-keys (lambda (k) (push k result)) hash-table)
    (values result)))

(defun hash-table-values (hash-table)
  (let ((result (list)))
    (map-hash-values (lambda (v) (push v result)) hash-table)
    (values result)))

(defun factorial (n)
  (if (or (zerop n)
	  (= n 1))
      1
      (* n (factorial (1- n)))))

(defun std-combination-size (n k)
  (/ (factorial n)
     (* (factorial k)
	(factorial (- n k)))))

(defun sorted (sequence predicate &key key)
  (let ((clone (copy-seq sequence)))
    (sort clone predicate :key key)))
