(in-package :lisperati)

(defvar *inner-template* nil)

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
  (let ((files (mapcar #'princ-to-string (list-all-files (eval directory) :match match))))
    (append (list'progn)
            (loop for file in files
               collect `(define-renderer ,file :prefix ,prefix :postfix ,postfix :dirs-in-name ,dirs-in-name)))))

(defmacro define-renderer-with-inner-template (outer-file inner-file
                                               &key (dirs-in-name 1)
                                                 (template-in-renderer '*inner-template*)
                                                 prefix
                                                 postfix)
  (let ((fname (filename-to-renderer-name (eval inner-file) :dirs-in-name dirs-in-name :prefix prefix :postfix postfix))
        (outer-fname (filename-to-renderer-name (eval outer-file))))
    `(progn
       (define-renderer ,outer-file)
       (let ((template (compile-file-template ,inner-file)))
         (defun ,fname ()
           (let ((,template-in-renderer template))
             (funcall (function ,outer-fname))))))))

(defmacro defrenderer-with-inner-template (outer-file inner-files
                                           &key (template-in-renderer '*inner-template*)
                                             match
                                             prefix
                                             postfix
                                             (dirs-in-name 1))
  (let ((files (mapcar #'princ-to-string (list-all-files (eval inner-files) :match (eval match) :exclude (truename (eval outer-file))))))
    (cons 'progn
          (loop for file in files
             collect `(define-renderer-with-inner-template ,outer-file ,file
                        :template-in-renderer ,template-in-renderer
                        :prefix ,prefix
                        :postfix ,postfix
                        :dirs-in-name ,dirs-in-name)))))

(defun list-all-files (directory &key (match ".") exclude)
  (let ((files))
    (cl-fad:walk-directory
     directory
     (lambda (file)
       (when (and
              (scan match (princ-to-string file))
              (or (not exclude)
                  (not (scan (princ-to-string exclude) (princ-to-string file)))))
         (push file files))))
    files))
