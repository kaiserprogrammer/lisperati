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
                                                 :dirs-in-name 0)))
  (is (eql 'render-lisperati-sub-sub_relative
           (lisperati::filename-to-renderer-name (relative-file "sub/sub_relative.test")
                                                 :dirs-in-name 2))))

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
    (fmakunbound 'render-test)
    (define-renderer (relative-file "test.lr") :dirs-in-name 0)
    (let ((*counter* 1))
      (is (string= "blub1" (render-test)))))
  (finishes
    (fmakunbound 'render-lisperati-test)
    (define-renderer (relative-file "test.lr"))
    (let ((*counter* 2))
      (is (string= "blub2" (render-lisperati-test)))))
  (finishes
    (fmakunbound 'render-blub-lisperati-test)
    (define-renderer (relative-file "test.lr") :prefix "blub")
    (let ((*counter* 3))
      (is (string= "blub3" (render-blub-lisperati-test)))))
  (finishes
    (fmakunbound 'render-lisperati-test-blub)
    (define-renderer (relative-file "test.lr") :postfix "blub")
    (let ((*counter* 3))
      (is (string= "blub3" (render-lisperati-test-blub))))))

(test defrenderer
  (finishes
    (fmakunbound 'render-blub-lisperati-sub-sub_relative-defrenderer1)
    (fmakunbound 'render-blub-sub-sub-sub_relative-defrenderer1)
    (defrenderer (relative-file "sub") :match ".*\\.test$" :prefix "blub" :postfix "defrenderer1" :dirs-in-name 2)
    (is (string= "true" (render-blub-lisperati-sub-sub_relative-defrenderer1)))
    (is (string= "true" (render-blub-sub-sub-sub_relative-defrenderer1)))))

(test define-renderer-with-inner-template
  (finishes
    (fmakunbound 'render-rec-inner)
    (define-renderer-with-inner-template (relative-file "outer.lr") (relative-file "rec/inner.lr"))
    (is (string= "outer inner outer" (render-rec-inner))))
  (finishes
    (fmakunbound 'render-inner)
    (define-renderer-with-inner-template (relative-file "outer.lr") (relative-file "rec/inner.lr") :dirs-in-name 0)
    (is (string= "outer inner outer" (render-inner))))
  (finishes
    (fmakunbound 'render-blub-inner)
    (define-renderer-with-inner-template (relative-file "outer.lr") (relative-file "rec/inner.lr") :dirs-in-name 0 :prefix "blub")
    (is (string= "outer inner outer" (render-blub-inner))))
  (finishes
    (fmakunbound 'render-inner-blub)
    (define-renderer-with-inner-template (relative-file "outer.lr") (relative-file "rec/inner.lr") :dirs-in-name 0 :postfix "blub")
    (is (string= "outer inner outer" (render-inner-blub)))))

(test defrenderer-with-inner-template
  (finishes
    (fmakunbound 'render-blub-lisperati-sub-sub_relative-defrenderer1)
    (fmakunbound 'render-blub-sub-sub-sub_relative-defrenderer1)
    (defrenderer-with-inner-template (relative-file "outer.lr")
        (relative-file "sub") :match ".*\\.test$" :prefix "blub" :postfix "defrenderer1" :dirs-in-name 2)
    (is (string= "outer true outer" (render-blub-lisperati-sub-sub_relative-defrenderer1)))
    (is (string= "outer true outer" (render-blub-sub-sub-sub_relative-defrenderer1)))))

(test defrenderer-does-not-recursive-define-outer-renderer
  (finishes
    (fmakunbound 'render-outer)
    (defrenderer-with-inner-template "~/code/lisperati/outer.lr" "~/code/lisperati/" :dirs-in-name 0 :match "outer\\.lr$")
    (is (not (fboundp 'render-outer)))))

(run!)
