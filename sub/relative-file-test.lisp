(in-package :lisperati-test)

(def-suite relative-file)
(in-suite relative-file)

(test sub-relative-file
  (is (equal "true" (get-whole-file-as-string (relative-file "sub_relative.test"))))
  (is (equal "true" (get-whole-file-as-string (relative-file "../relative.test")))))

(run!)
