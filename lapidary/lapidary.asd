;;;								-*- Lisp -*-
;;; lapidary.asd - System definition for lapidary
;;;

(defsystem lapidary
    :name               "lapidary"
    :description        "Lapidary application builder."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   ""
    :depends-on (:garnet)
    :components
    :serial t
    ((:file "lapidary-functions")
     (:file "parameters")
     (:file "defs")
     (:file "macros")
     (:file "lapidary"  )
     (:file "dialog-parts2")
     (:file "event-card")
     (:file "card")
     (:file "card1")
     (:file "start-where")
     (:file "prompt")
     (:file "lapidary-objects")
     (:file "feedback-objs" )
     (:file "support-misc")
     (:file "support-selection1")
     (:file "support-selection2")
     (:file "selection")
     (:file "create-object")
     (:file "delete-object")
     (:file "delete-window")
     (:file "move-grow")
     (:file "aggregates")
     (:file "aggparam")
     (:file "create-parameters")
     (:file "properties")
     (:file "line-imp")
     (:file "line-props")
     (:file "fill-imp")
     (:file "fill-props")
     (:file "color-imp")
     (:file "color-props")
     (:file "shapes")
     (:file "lap-draw")
     (:file "support-menu-editor")
     (:file "new-editor")
     (:file "text")
     (:file "text-properties")
     (:file "gadgetcopy")
     (:file "save-link-parameters")
     (:file "lapidary-save")
     (:file "lapidary-read")
     (:file "support-save-restore")
     (:file "save-restore")
     (:file "add-gadget")
     (:file "choice-inter")
     (:file "text-inter")
     (:file "move-grow-box")
     (:file "support-move-grow-inter")
     (:file "move-grow-inter" )
     (:file "angle-inter")
     (:file "two-point-inter")
     (:file "support-inter")
     (:file "by-demo")
     (:file "interactors")
     (:file "interactors-menu")
     ))
