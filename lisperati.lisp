(defpackage :lisperati
  (:use :cl :cl-ppcre)
  (:export
   :compile-template
   :render-template
   :compile-file-template
   :relative-file
   :get-whole-file-as-string
   :inline-template
   :inline-file-template
   :defrenderer
   :defrenderer-with-page
   :define-renderer-with-page
   :define-renderer))
(in-package :lisperati)

(defun compile-file-template (file)
  (let ((content (get-whole-file-as-string (pathname file))))
    (compile-template content)))

(defun compile-template (template)
  (let ((snippets (find-snippets template)))
    (compile nil `(lambda () (concatenate 'string ,@snippets)))))

(defmacro inline-template (template)
  (let ((snippets (find-snippets (eval template))))
    `(concatenate 'string ,@snippets)))

(defmacro inline-file-template (file)
  (let ((snippets (find-snippets (get-whole-file-as-string (pathname (eval file))))))
    `(concatenate 'string ,@snippets)))

(defun find-snippets (template)
  (apply
   #'append
   (let ((found (scan "(\\(=|\\(\\()" template)))
     (if found
         (let ((string-found (subseq template found (+ found 2))))
           (if (string= string-found "(=")
               (insert-print-template template found)
               (insert-logic-template template found)))
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

(defun insert-print-template (template found)
  (multiple-value-bind (exp length)
      (read-from-string (subseq template (+ found 2)))
    `((,(subseq template 0 found)
        (princ-to-string ,exp))
      ,(find-snippets (subseq template (+ found length 3))))))

(defun insert-logic-template (template found)
  `((,(subseq template 0 found)
      ,@(insert-recursive-template (subseq template found)))))

(defun insert-recursive-template (template)
  (let ((compiled-template
         (concatenate
          'string
          "(with-output-to-string (s) "
          (insert-inner-template (subseq template 1)))))
    (multiple-value-bind (exp clength)
        (read-from-string compiled-template)
      (declare (ignore clength))
      (multiple-value-bind (whole length)
          (read-from-string template)
        (declare (ignore whole))
        `(,exp
          ,@(find-snippets (subseq template length)))))))

(defun insert-inner-template (template)
  (let ((template-found (scan "\\(with-template" template)))
    (if (not template-found)
        template
        (multiple-value-bind (exp inner-length)
            (read-from-string (subseq template template-found))
          (declare (ignore exp))
          (let* ((offset (if (and (or (char= #\  (elt template (1- (+ template-found inner-length))))
                                      (char= #\Newline  (elt template (1- (+ template-found inner-length)))))
                                  (char= #\) (elt template (- (+ template-found inner-length) 2))))
                             -2
                             -1))
                 (blub (subseq template (+ 15 template-found) (+ offset (+ template-found inner-length)))))
            (concatenate
             'string
             (subseq template 0 template-found)
             (format nil "(format s \"~~{~~a~~}\" ~w)"
                     (cons 'list (find-snippets blub)))
             (insert-inner-template (subseq template (+ 15 template-found (- (length blub) offset))))))))))

(defvar *inner-template* nil)

(defmacro defrenderer (dir)
  (let ((files (mapcar #'princ-to-string (cl-fad:list-directory (eval dir)))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer ,file)))))

(defmacro defrenderer-with-page (dir renderer &optional (template-in-renderer '*inner-template*))
  (let ((files (mapcar #'princ-to-string (cl-fad:list-directory (eval dir)))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer-with-page ,file ,renderer ,template-in-renderer)))))

(defmacro define-renderer-with-page (filename renderer &optional (template-in-renderer '*inner-template*))
  (let ((fname (filename-to-renderer-name filename)))
    `(let ((template (compile-file-template ,filename)))
       (defun ,fname ()
         (let ((,template-in-renderer template))
           (funcall ,renderer))))))

(defun filename-to-renderer-name (filename)
  (let* ((file filename)
       (dirs (split "/" file))
       (dir (elt dirs (- (length dirs) 2)))
       (action (elt (split "\\." (elt dirs (1- (length dirs)))) 0))
       (fname (intern (string-upcase (concatenate 'string
                                                  "render-"
                                                  dir
                                                  "-"
                                                  action)))))
    fname))

(defmacro define-renderer (filename)
  (let* ((fname (filename-to-renderer-name filename)))
    `(let ((template (compile-file-template ,filename)))
       (defun ,fname
           ()
         (render-template template)))))

