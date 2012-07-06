(defpackage :lisperati
  (:use :cl :cl-ppcre)
  (:export
   :compile-template
   :activate
   :insert-template))
(in-package :lisperati)

(defmacro compile-template (template)
  `(lambda () (concatenate 'string
                      ,@(find-snippets template))))

(defun find-snippets (template)
  (apply #'append (let ((found (scan "\\(=" template)))
      (if found
          (multiple-value-bind (exp length)
              (read-from-string (subseq template (+ found 2)))
            (list (list (subseq template 0 found)
                        (list 'princ-to-string exp))
                  (find-snippets (subseq template (+ found length 3)))))
          (list (list template))))))

(defun insert-template (compiled-template)
  (funcall compiled-template))
