(defpackage :renderer-generators-test
  (:use :cl :fiveam :lisperati))
(in-package :renderer-generators-test)

(def-suite renderer-generators)
(in-suite renderer-generators)

(test file-does-not-exist
  (signals (sb-int:simple-file-error)
    (lisperati::filename-to-renderer-name "does_not_exist"))
  (signals (type-error)
    (lisperati::filename-to-renderer-name nil)))

(test renderer-generators-length
  (is (eql 'render-sub-sub_relative
           (lisperati::filename-to-renderer-name (relative-file "sub/sub_relative.test"))))
  (is (eql 'render-sub_relative
           (lisperati::filename-to-renderer-name (relative-file "sub/sub_relative.test")
                                                 :length 0)))
  (is (eql 'render-lisperati-sub-sub_relative
           (lisperati::filename-to-renderer-name (relative-file "sub/sub_relative.test")
                                                 :length 2))))

(test renderer-generators-prefix
  (is (eql 'render-blub-sub-sub_relative
           (lisperati::filename-to-renderer-name (relative-file "sub/sub_relative.test")
                                                 :prefix "blub"))))

(test renderer-generators-postfix
  (is (eql 'render-sub-sub_relative-blub
           (lisperati::filename-to-renderer-name (relative-file "sub/sub_relative.test")
                                                 :postfix "blub"))))
(defvar *counter*)
(test define-renderer
  (finishes
    (define-renderer (relative-file "test.lr") :length 0)
    (let ((*counter* 1))
      (is (string= "blub1" (render-test)))))
  (finishes
    (define-renderer (relative-file "test.lr"))
    (let ((*counter* 2))
      (is (string= "blub2" (render-lisperati-test)))))
  (finishes
    (define-renderer (relative-file "test.lr") :prefix "blub")
    (let ((*counter* 3))
      (is (string= "blub3" (render-blub-lisperati-test)))))
  (finishes
    (define-renderer (relative-file "test.lr") :postfix "blub")
    (let ((*counter* 3))
      (is (string= "blub3" (render-lisperati-test-blub))))))

(test defrenderer
  (finishes
    (defrenderer "~/code/lisperati/sub/" :match ".*\\.test$" :prefix "blub" :postfix "defrenderer1" :length 2)
    (is (string= "true" (render-blub-lisperati-sub-sub_relative-defrenderer1)))
    (is (string= "true" (render-blub-sub-sub-sub_relative-defrenderer1)))))

(run!)
