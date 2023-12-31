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
         2/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
         6/16/92 Rajan Parthasarathy - Added load of scrolling-window-multifont
         4/15/92 Andrew Mickish   - Added load of ps-multifont
         4/01/92 Richard McDaniel - Created
==================================================================
|#

(in-package "USER")

;; check first to see if places are set
(unless (boundp 'Garnet-Opal-PathName)
   (error "Load 'Multifont-Loader' first to set Garnet-Opal-PathName before loading Gadgets.")
)

(unless (boundp 'Garnet-Inter-PathName)
   (error "Load 'Multifont-Loader' first to set Garnet-Inter-PathName before loading Gadgets.")
)

;;; Load modules required for multifont
;;;

(unless (get :garnet-modules :multifont)
   (format t "Loading Multifont...~%")
   (load (user::garnet-pathnames "multifont"
               #+cmu "opal:"
               #+(not cmu) Garnet-Opal-PathName)
         :verbose T)
   
   (dolist (file (list "lispkeyhandling"
		       "multifont-textinter"
		       "focus-multifont-textinter"
		       "selection-interactor"))
     (load (user::garnet-pathnames file
			    #+cmu "inter:"
			    #+(not cmu) Garnet-Inter-PathName)
	   :verbose T))
   
   ; Load special printing functions for multifont if PS module already loaded
   ; (otherwise they will be loaded by the PS loader when needed).
   (if (get :garnet-modules :ps)
       (load (user::garnet-pathnames "ps-multifont"
		   #+cmu "ps:"
		   #+(not cmu) Garnet-PS-Pathname)
	     :verbose T))

   ; Load special scrolling functions for multifont if PS module already loaded
   ; (otherwise they will be loaded by the PS loader when needed).
   (if (get :garnet-modules :scrolling-window)
       (load (user::garnet-pathnames "scrolling-window-multifont"
		   #+cmu "gadgets:"
		   #+(not cmu) Garnet-Gadgets-Pathname)
	     :verbose T))
   
   (format t "...Done Multifont.~%")
)

(setf (get :garnet-modules :multifont) t)
