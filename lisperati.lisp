(defpackage :lisperati
  (:use :cl :cl-ppcre)
  (:export
   :compile-template
   :render-template
   :compile-file-template
   :relative-file
   :get-whole-file-as-string))
(in-package :lisperati)

(defun compile-file-template (file)
  (let ((content (get-whole-file-as-string (pathname file))))
    (compile-template content)))

(defun compile-template (template)
  (let ((snippets (find-snippets template)))
    (compile nil `(lambda () (concatenate 'string ,@snippets)))))

(defun find-snippets (template)
  (apply
   #'append
   (let ((found (scan "(\\(=|\\(\\()" template)))
     (if found
         (let ((string-found (subseq template found (+ found 2))))
           (if (string= string-found "(=")
               (multiple-value-bind (exp length)
                   (read-from-string (subseq template (+ found 2)))
                 `((,(subseq template 0 found)
                     (princ-to-string ,exp))
                   ,(find-snippets (subseq template (+ found length 3)))))
               (let ((template-found (scan "\\(with-template" template :start found)))
                 (if template-found
                     (multiple-value-bind (exp inner-length)
                         (read-from-string (subseq template template-found))
                       (declare (ignore exp))
                       (let ((compiled-template
                              (concatenate
                               'string
                               "(with-output-to-string (s) "
                               (subseq template (1+ found) template-found)
                               (let* ((offset (if (char= #\)
                                                         (elt template (1- (+ template-found inner-length))))
                                                  -1
                                                  -2))
                                      (blub (subseq template (+ 15 template-found) (+ offset (+ template-found inner-length)))))
                                 (format nil "(format s \"~~{~~a~~}\" ~w)"
                                         (cons 'list (find-snippets blub))))
                               (subseq template (+ template-found inner-length)))))
                         (multiple-value-bind (exp clength)
                             (read-from-string compiled-template)
                           (declare (ignore clength))
                           (multiple-value-bind (whole length)
                               (read-from-string (subseq template found))
                             (declare (ignore whole))
                            `((,(subseq template 0 found)
                                ,exp
                                ,@(find-snippets (subseq template (+ found length)))))))))
                     (multiple-value-bind (exp length)
                         (read-from-string (subseq template found))
                       `((,(subseq template 0 found)
                           (with-output-to-string (s)
                             ,exp)
                           ,(find-snippets (subseq template (+ found length 2))))))))))
         (list (list template))))))

(defun render-template (compiled-template)
  (funcall compiled-template))

(defun get-whole-file-as-string (name)
  (with-open-file (in name)
    (let* ((length (file-length in))
           (text (make-string length))
           (read (read-sequence text in)))
      (if (< read length)
          (subseq text 0 read)
          text))))

(defmacro relative-file (name)
  `(make-pathname
    :name ,name
    :directory
    (pathname-directory
     (or ,*load-truename*
         ,*compile-file-truename*))))

