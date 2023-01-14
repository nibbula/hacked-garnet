;;;								-*- Lisp -*-
;;; c32.asd - System definition for c32
;;;

(defsystem c32
    :name               "c32"
    :description        "C32 constraint spreadsheet."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "GPL-3.0-only"
    :source-control	:git
    :long-description   "C32 constraint spreadsheet."
    :depends-on (:garnet)
    :components ((:file "c32")
		 (:file "c32formula")
		 (:file "c32ref")
		 (:file "pop-up-generalize")
		 (:file "pop-up-copy-formula")
		 (:file "pop-up-ask-object")
		 (:file "pop-up-functions")
		 (:file "c32dialog")
		 (:file "c32-lapidary")))
