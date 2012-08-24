(cl:defpackage :lisperati-systems
  (:use :cl :asdf))
(cl:in-package :lisperati-systems)

(defsystem :lisperati
  :version "0.1"
  :author "Jürgen Bickert <juergenbickert@gmail.com>"
  :maintainer "Jürgen Bickert <juergenbickert@gmail.com>"
  :description "A fast templating engine"
  :depends-on (:cl-ppcre
               :cl-fad)
  :components ((:file "lisperati")
               (:file "renderer-generators")))
