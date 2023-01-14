;;;								-*- Lisp -*-
;;; garnet-demos.asd - System definition for garnet-demos
;;;

(defsystem garnet-demos
    :name               "garnet-demos"
    :description        "Demos for Garnet."
    :version            "0.1.0"
    :license            "Unlicense"
    :source-control	:git
    :long-description   "Demos for Garnet."
    :depends-on (:garnet)
    :serial t
    :components
    ((:file "demos-controller")))
