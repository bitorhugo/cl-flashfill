(in-package :cl-flashfill)

(defun curry (fn &rest args)
  #'(lambda (&rest more-args)
      (apply fn (append args more-args))))

(defun rcurry (fn &rest args)
  #'(lambda (&rest more-args)
      (apply fn (append more-args args))))

(defun rac (list)
  (car (last list)))
