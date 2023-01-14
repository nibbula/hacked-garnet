;;;								-*- Lisp -*-
;;; gilt.asd - System definition for gilt
;;;

(defsystem gilt
    :name               "gilt"
    :description        "Gilt interface builder."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "Gilt interface builder."
    :depends-on (:garnet)
    :serial t
    :components
    ((:flie "gilt-functions")
     (:flie "filter-functions")
     (:flie "path-functions")
     (:flie "gilt-gadget-utils")
     (:flie "motif-gilt-gadgets")
     (:flie "gilt-gadgets")
     (:flie "gilt")
     (:flie "line-imp")
     (:flie "motif-line-props")
     (:flie "fill-imp")
     (:flie "motif-fill-props")
     (:flie "align")
     (:flie "motif-gilt-save")
     (:flie "motif-gilt-read")
     (:flie "gilt-font-imp")
     (:flie "motif-gilt-font-props")
     (:flie "color-imp")
     (:flie "motif-color-props")
     (:flie "value-control")
     (:flie "enable-control")
     (:flie "error-check")))
