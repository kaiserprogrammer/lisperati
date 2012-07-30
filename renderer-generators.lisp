(in-package :lisperati)

(defun filename-to-renderer-name (filename &key (dirs-in-name 1) prefix postfix)
  (let* ((file (princ-to-string (truename filename)))
         (dirs (split "/" file))
         (subnames (subseq dirs (- (length dirs) (1+ dirs-in-name)) (1- (length dirs))))
         (action (elt (split "\\." (elt dirs (1- (length dirs)))) 0)))
    (when prefix
      (push prefix subnames))
    (setf subnames (append subnames (list action)))
    (when postfix
      (setf subnames (append subnames (list postfix))))
    (intern (string-upcase (format nil "render~{-~a~^~}" subnames)))))

(defmacro define-renderer (filename &key (dirs-in-name 1) prefix postfix)
  (let* ((fname (filename-to-renderer-name (eval filename) :dirs-in-name (eval dirs-in-name) :prefix prefix :postfix postfix)))
    `(let ((template (compile-file-template ,filename)))
       (defun ,fname
           ()
         (render-template template)))))

(defmacro defrenderer (directory &key match prefix postfix (dirs-in-name 1))
  (let ((files (mapcar #'princ-to-string (list-all-files directory :match match))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer ,file :prefix ,prefix :postfix ,postfix :dirs-in-name ,dirs-in-name)))))

(defun list-all-files (directory &key (match "."))
  (remove-if-not
   (lambda (file) (scan match (princ-to-string file)))
   (let ((dirs))
     (cl-fad:walk-directory
      (eval directory)
      (lambda (dir)
        (push dir dirs)))
     dirs)))
