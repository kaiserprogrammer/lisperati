(defpackage :lisperati-test
  (:use :cl :fiveam :lisperati))
(in-package :lisperati-test)

(def-suite lisperati)
(in-suite lisperati)

(test no-substitution
  (let ((templ "blub"))
    (is (equal "blub" (render-template (compile-template templ))))))

(test with-subsititution
  (is (equal "blub1" (render-template (compile-template "blub(=1)")))))

(test with-binding
  (let* ((counter 2)
         (template "blub(=counter)"))
    (declare (special counter))
    (is (equal "blub2" (render-template (compile-template template))))))

(test multiple-substitution
  (let ((counter 1))
    (declare (special counter))
    (is (equal "blub1blub2blub3"
               (render-template (compile-template
                "blub(=counter)blub(=(incf counter))blub(=(incf counter))"))))))

(test lisp-mix
  (is (equal
       "<div class=\"1\"></div><div class=\"2\"></div><div class=\"3\"></div>"
       (render-template (compile-template
        "((loop for i in (list 1 2 3)
              do (with-template <div class=\"(=i)\"></div>)))")))))

(test file-templates
  (let ((*counter* 1))
    (declare (special *counter*))
    (is (equal "blub1"
               (render-template (compile-file-template (relative-file "test.lr"))))))
  (let ((*counter* 2)
        (file (relative-file "test.lr")))
    (declare (special *counter*))
    (is (equal "blub2"
               (render-template (compile-file-template file))))))

(test relative-file
  (is (equal "true" (get-whole-file-as-string (relative-file "relative.test"))))
  (is (equal "true" (get-whole-file-as-string (relative-file "sub/sub_relative.test")))))

(run!)
