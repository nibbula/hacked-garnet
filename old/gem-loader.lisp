;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Changes:
;;;  1-Nov-93 Mickish - Created

(in-package "USER")

(defparameter Gem-Version-Number "1.0")

(format t "Loading Gem...~%")

;;; check to see if pathname variable is set
(unless (boundp 'Garnet-Gem-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gem-PathName before loading gem."))

;;;  Load Gem  ...
(Defvar Garnet-Gem-Files
  '(
    "gem"
    "define-methods"
    ))

(unless (get :garnet-modules :gem)
  (dolist (file Garnet-Gem-Files)
    (load (user::garnet-pathnames file Garnet-Gem-PathName)
	  :verbose T)))

(setf (get :garnet-modules :gem) t)
(format t "...Done Gem.~%")
