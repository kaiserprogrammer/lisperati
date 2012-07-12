(defpackage :lisperati-test
  (:use :cl :fiveam :lisperati))
(in-package :lisperati-test)

(def-suite lisperati)
(in-suite lisperati)

(test no-substitution
  (let ((templ "blub"))
   (is (equal "blub" (insert-template (compile-template templ))))))

(test with-subsititution
  (is (equal "blub1" (insert-template (compile-template "blub(=1)")))))

(test with-binding
  (let* ((*counter* 2)
        (template "blub(=*counter*)")
         (compiled-template (compile-template template)))
    (declare (special *counter*))
    (is (equal "blub2" (insert-template compiled-template)))))

(test multiple-substitution
  (let ((*counter* 1))
    (declare (special *counter*))
    (is (equal "blub1blub2blub3"
               (insert-template
                (compile-template
                 "blub(=*counter*)blub(=(incf *counter*))blub(=(incf *counter*))"))))))

(test lisp-mix
  (is (equal
       "<div class=\"1\"></div><div class=\"2\"></div><div class=\"3\"></div>"
       (insert-template
        (compile-template
         "((loop for i in (list 1 2 3)
              do (with-template <div class=\"(=i)\"></div>)))")))))

(test file-templates
  (let ((compiled-template (compile-file-template (make-pathname
                                                   :name "test.lr"
                                                   :directory
                                                   (pathname-directory
                                                    (pathname
                                                     (or *compile-file-truename*
                                                         *load-truename*))))))
        (*counter* 1))
    (declare (special *counter*))
    (is (equal "blub1" (insert-template compiled-template)))))

(test relative-file
  (is (equal "true" (get-whole-file-as-string (relative-file "relative.test"))))
  (is (equal "true" (get-whole-file-as-string (relative-file "sub/sub_relative.test")))))

(run!)
