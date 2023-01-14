;;;								-*- Lisp -*-
;;; agate.asd - System definition for agate
;;;

(defsystem agate
    :name               "agate"
    :description        "Gesture trainer application."
    :version            "0.1.0"
    :author             "Nibby Nebbulous <nibbula -(. @ .)- uucp!gmail.com>"
    :license            "Unlicense"
    :source-control	:git
    :long-description
    "This is AGATE: The Garnet Gesture Trainer application. It is used for
training gestures that are passed to the Garnet gesture interactor."
    :depends-on (:garnet)
    :components
    ((:file "train")
     (:file "agate")))
