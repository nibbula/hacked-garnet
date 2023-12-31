;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Changes:
;;;     2/24/93 Andrew Mickish - Removed references to compile-opal/inter-p
;;;    10/01/92 Andrew Mickish - Removed *garnet-going-to-compile*
;;;     5/22/92 Brad Vander Zanden - Added kr-changes.lisp
;;; 	5/4/92 Ed Pervin - Changed Garnet-Lapidary-Pathname to 
;;;			  Garnet-Lapidary-Src.  Added "mouse-bindings"
;;;			  to list of files.
;;;

(in-package "USER")

;; Only loads this file when not compiling all of Garnet.
(unless (get :garnet-modules :multifont)
  (garnet-load "opal:multifont-loader"))

(unless (get :garnet-modules :debug)
  (garnet-load "debug:debug-loader"))

;; load necessary gadgets
(dolist (pair '((:text-buttons "text-buttons-loader")
		(:error-gadget "error-gadget-loader")
		(:arrow-line "arrow-line-loader")
		(:labeled-box "labeled-box-loader")
		(:x-buttons "x-buttons-loader")
		(:v-slider "v-slider-loader")
		(:scrolling-labeled-box "scrolling-labeled-box-loader")
		(:radio-buttons "radio-buttons-loader")
		(:scrolling-window "scrolling-window-loader")
		(:scrolling-menu "scrolling-menu-loader")
		(:menubar "menubar-loader")
		(:prop-sheet-win "prop-sheet-win-loader")))
  (unless (get :garnet-modules (car pair))
    (garnet-load (concatenate 'string "gadgets:" (cadr pair)))))

(unless (get :garnet-modules :gilt-functions)
  (garnet-load "gilt:gilt-functions-loader"))
(unless (get :garnet-modules :path-functions)
  (garnet-load "gilt:path-functions-loader"))
(unless (get :garnet-modules :c32)
  (load user::garnet-c32-loader))

;;; Compile and load the constraint gadget

(defvar Garnet-Constraint-Gadget-Pathname
  (user::garnet-pathnames #-apple "lapidary/" #+apple "lapidary:"
                    Garnet-Binary-Pathname))
(defvar Garnet-Constraint-Gadget-Src
  (user::garnet-pathnames #-apple "lapidary/" #+apple "lapidary:"
                    Garnet-Src-Pathname))

(setf Garnet-Constraint-Gadget-Pathname Garnet-Constraint-Gadget-Src)

;;; If at cmu, then set up the search lists
#+cmu
(progn
  (setf (ext:search-list "constraint-gadget:")
	(list (namestring Garnet-Constraint-Gadget-PathName)))
  (setf (ext:search-list "constraint-gadget-src:")
	(list (namestring Garnet-Constraint-Gadget-Src))))

(defparameter Garnet-Constraint-Gadget-Compiler
  (user::garnet-pathnames "constraint-gadget-compiler" 
		    Garnet-Constraint-Gadget-PathName))

(load Garnet-Constraint-Gadget-Compiler)

(Defparameter Garnet-Lapidary-Files
  '(
        "lapidary-functions"
	"mouse-bindings"
	"parameters"
        "defs"
        "macros"
	"lapidary"  
	"dialog-parts2" "event-card" "card" "card1"
	"start-where" "prompt"
        "lapidary-objects"
	"feedback-objs"
	"support-misc"
	"support-selection1"	"support-selection2"   	"selection"
	"create-object"
	"delete-object"
        "delete-window"
	"move-grow"
	"aggregates"
	"aggparam" "create-parameters"
	"properties"
        "line-imp" "line-props"
	"fill-imp" "fill-props"
	"color-imp" "color-props"
	"shapes"
	"lap-draw"
	"support-menu-editor"		        "new-editor"
        "text"   "text-properties"
        "gadgetcopy"
	"save-link-parameters"
        "lapidary-save"	"lapidary-read"	"support-save-restore" "save-restore"
	"add-gadget"
	"choice-inter" "text-inter"
	"move-grow-box" "support-move-grow-inter" "move-grow-inter" 
	"angle-inter" "two-point-inter"
	"support-inter" "by-demo"
	"interactors" "interactors-menu"))

(dolist (file Garnet-Lapidary-Files)
  (garnet-compile (concatenate 'string "lapidary-src:" file))
  (garnet-load (concatenate 'string "lapidary-src:" file)))
