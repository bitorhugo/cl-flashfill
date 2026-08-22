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


;; tests

(let ((name "John Smith"))
  ;; John
  (format t "~a~%" (sub-str "John Smith" 0 4))

  ;; J. Smith -> J dot space Smith
  (format t "~a~%" (concat (sub-str "John Smith" 0 1)
			   (literal ".")
			   (literal " ") ; we could use sub-str starting from 4 as well
			   (sub-str "John Smith" 5))))


(let ((name "Jane Doe"))
  (format t "~a~%" (concat (sub-str name 0 1)
			   (literal ".")
			   (literal " ")
			   (sub-str name 5))))
