(defpackage :lisperati
  (:use :cl :cl-ppcre)
  (:export
   :compile-template
   :insert-template
   :compile-file-template
   :relative-file
   :get-whole-file-as-string))
(in-package :lisperati)

(defun compile-file-template (file)
  (compile-template (get-whole-file-as-string (pathname file))))

(defun compile-template (template)
  `(concatenate 'string ,@(find-snippets template)))

(defun find-snippets (template)
  (apply
   #'append
   (let ((found (scan "(\\(=|\\(\\()" template)))
     (if found
         (let ((string-found (subseq template found (+ found 2))))
           (if (string= string-found "(=")
               (multiple-value-bind (exp length)
                   (read-from-string (subseq template (+ found 2)))
                 (print`((,(subseq template 0 found)
                      (princ-to-string ,exp))
                    ,(find-snippets (subseq template (+ found length 3))))))
               (let ((template-found (scan "\\(with-template" template :start found)))
                 (if template-found
                     (multiple-value-bind (exp inner-length)
                         (read-from-string (subseq template template-found))
                       (let ((compiled-template
                              (concatenate
                               'string
                               "(with-output-to-string (s) "
                               (subseq template (1+ found) template-found)
                               (format nil "(format s ~w)" (compile-template (subseq template (+ 15 template-found) (1- (+ template-found inner-length)))))
                               (subseq template (+ template-found inner-length)))))
                         (print compiled-template)
                         (print template)
                         (print (+ found inner-length))
                         (multiple-value-bind (exp length)
                             (read-from-string compiled-template)
                           (print `((,(subseq template 0 found)
                                     ,exp
                                     ,@(find-snippets (subseq template (+ template-found inner-length 2)))))))))
                     (multiple-value-bind (exp length)
                         (read-from-string (subseq template found))
                       (progn (print exp)
                        `((,(subseq template 0 found)
                            (with-output-to-string (s)
                              ,exp)
                            ,(find-snippets (subseq template (+ found length 2)))))))))))
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




;; (with-output-to-string (s)
;;   (loop for i in (list 1 2 3)
;;      do (format s (concatenate 'string
;;                                "<div class=\""
;;                                (princ-to-string i)
;;                                "\"></div>"))))
