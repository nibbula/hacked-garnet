;;; -*- Mode: Lisp, Fill, Save; Package: USER -*-
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
         1/26/90 Andrew Mickish - Created
==================================================================
|#

(in-package "USER")

;; check first to see if place is set
(unless (boundp 'Garnet-Gadgets-PathName)
  (error "Load 'Garnet-Loader' first to set Garnet-Gadgets-PathName before loading Gadgets."))

(unless (get :garnet-modules :browser-gadget)
  (format t "Loading Browser-Gadget...~%")
  (dolist (pair '((:h-scroll-bar "h-scroll-loader")
		  (:scrolling-menu "scrolling-menu-loader")
		  (:browser-gadget "browser-gadget")))
    (unless (get :garnet-modules (car pair))
      (load (user::garnet-pathnames (cadr pair)
			     #+cmu "gadgets:"
			     #+(not cmu) Garnet-Gadgets-PathName)
	    :verbose T)))
  (format t "...Done Browser-Gadget.~%"))


(setf (get :garnet-modules :browser-gadget) t)

