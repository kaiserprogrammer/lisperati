(defpackage :lisperati-test
  (:use :cl :fiveam :lisperati))
(in-package :lisperati-test)

(def-suite lisperati)
(in-suite lisperati)

(test no-substitution
  (is (equal "blub" (insert-template (compile-template "blub")))))

(test with-subsititution
  (is (equal "blub1" (insert-template (compile-template "blub(=1)")))))

(test with-binding
  (let ((counter 2))
    (is (equal "blub2" (insert-template (compile-template "blub(=counter)"))))))

(test multiple-substitution
  (let ((counter 1))
    (is (equal "blub1blub2blub3"
               (insert-template
                (compile-template
                 "blub(=counter)blub(=(incf counter))blub(=(incf counter))"))))))

(run!)
