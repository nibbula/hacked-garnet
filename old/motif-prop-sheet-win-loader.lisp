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

#|
==================================================================
Change log:
     03/05/91 Andrew Mickish - Created
==================================================================
|#

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

;;; Now load the prop-sheet-win module
;;;
(unless (get :garnet-modules :motif-prop-sheet-win)
  (format t "Loading Motif-Prop-Sheet-Win...~%")
  (dolist (pair '((:motif-text-buttons "motif-text-buttons-loader")
		  (:error-gadget-utils "error-gadget-utils")
		  (:prop-value "prop-value-loader")
		  (:prop-sheet "prop-sheet-loader")
		  (:motif-prop-sheet-win "motif-prop-sheet-win")))
    (unless (get :garnet-modules (car pair))
      (load (user::garnet-pathnames (cadr pair)
                             #+cmu "gadgets:"
                             #+(not cmu) Garnet-Gadgets-PathName)
            :verbose T)))
  (format t "...Done Motif-Prop-Sheet-Win.~%"))

(setf (get :garnet-modules :motif-prop-sheet-win) t)


