(defpackage :lisperati
  (:use :cl :cl-ppcre)
  (:export
   :compile-template
   :activate
   :insert-template
   :compile-file-template
   :relative-file
   :get-whole-file-as-string))
(in-package :lisperati)

(defun compile-file-template (file)
  (compile-template (get-whole-file-as-string (pathname file))))

(defmacro compile-template (template)
  `(let ((temp ,@(list template)))
     `(concatenate 'string ,@(find-snippets temp))))

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
  (eval compiled-template))

(defun get-whole-file-as-string (name)
  (with-open-file (in name)
    (let* ((length (file-length in))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
          (subseq text 0 read)
          text))))

(defun relative-file (name)
  (make-pathname
   :name name
   :directory
   (pathname-directory
    (or *load-truename*
        *compile-file-truename*))))
