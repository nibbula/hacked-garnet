;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: DEMOS-CONTROLLER; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file contains the main demos controller
;;;
;;; ** Call (Do-Go) to start and (Do-Stop) to stop **
;;;
;;; Designed and implemented by Osamu Hashimoto
;;;
;;;
;;; 29-Sep-93 Andrew Mickish - Added Demo-Unistrokes
;;; 12-Aug-93 Andrew Mickish - Put garnet-processes switch on wait-amount
;;; 16-Jul-93 Brad Myers - added mouseline popup
;;; 17-Feb-93 Andrew Mickish - Removed demo-twop, demo-sequence, demo-moveline,
;;;             and demo-array.  Changed demo-circle to demo-virtual-agg.
;;; 02-Feb-93 Andrew Mickish - opal:set-strings ---> opal:set-text;
;;;                            demo-calculator ---> garnet-calculator
;;; 27-Oct-92 Mickish - Added export to work around CMUCL bug
;;;  4-Jun-92 Myers/Pervin - Added demo-animator; changed "animate" to "logo".
;;;  13-Apr-92 Brad Myers -  Changed demo-fade to demo-logo
;;;  3-Apr-92 Mickish - Added Demo-Gesture
;;;  2-Apr-92 McDaniel - New multifont
;;; 30-Mar-92 Pervin - Added demo-circle, demo-array.
;;; 25-Feb-92 Pervin - Removed some unnecessary demos like mode, clock, truck.
;;;			Also, added some :constant slots.
;;; 13-Feb-92 Pervin - Merged color and non-color versions of demos.
;;; 10-Oct-91 Mickish - Added color-demo-fade
;;; 14-Mar-91 Mickish - Added demo-graph
;;; 13-Mar-91 Pervin Test whether using color screen.
;;;           If so, use color versions of demos.
;;; 13-Mar-91 Mickish - Added demo-motif and demo-truck
;;; 15-Nov-90 Pervin In Do-Stop, added test that item is not "animate".
;:;  5-Nov-90 Pervin In Garnet-Note-Quitted, added a test that win is boundp.


(in-package :DEMOS-CONTROLLER)

(declaim (special WIN1 AGG1 BT QBT DEMOS-MOUSELINE WIN2 TEXT))

;; Load multifont stuff.
(unless (get :garnet-modules :multifont)
   (load (user::garnet-pathnames "multifont-loader" user::Garnet-Opal-PathName)
         :verbose T))

(dolist (file '("x-buttons-loader"
		"text-buttons-loader"
		"scrolling-window-loader"
		"mouseline-loader"))
  (user::garnet-load (concatenate 'string "gadgets:" file)))

(user::garnet-load "demos:demo-logo")


;; export nothing, just work around a bug in CMUCL.
;; If didn't export here, CMUCL's overzealous optimizing compiler would
;; screw up the reference to demo-logo:do-go in the code below.
#+cmu (export '())

(defparameter *package-list*
   '(("3d" DEMO-3D)("angle" DEMO-ANGLE)("animator" DEMO-ANIMATOR)("arith" DEMO-ARITH)
     ("virtual-agg" DEMO-VIRTUAL-AGG)
     ("editor" DEMO-EDITOR)("file-browser" DEMO-FILE-BROWSER)
     ("gadgets" DEMO-GADGETS) ("garnetdraw" GARNETDRAW)("gesture" DEMO-GESTURE)
     ("grow" DEMO-GROW)("manyobjs" DEMO-MANYOBJS)
     ("menu" DEMO-MENU)("multifont" DEMO-MULTIFONT)
     ("multiwin" DEMO-MULTIWIN)("othello" DEMO-OTHELLO)("pixmap" DEMO-PIXMAP)
     ("schema-browser" DEMO-SCHEMA-BROWSER)
     ("scrollbar" DEMO-SCROLLBAR)("text" DEMO-TEXT)
     ("xasperate" DEMO-XASPERATE)
     ("calculator" GARNET-CALCULATOR)
     ("motif" DEMO-MOTIF) ("graph" DEMO-GRAPH)
     ("unistrokes" DEMO-UNISTROKES)))

(defparameter *running* NIL)

(defparameter *unloaded*
    '("3d" "angle" "animator" "arith" "calculator" "editor"
      "file-browser" "gadgets" "garnetdraw" "othello" "grow" "manyobjs" "menu"
      "multifont" "multiwin" "pixmap" "schema-browser" "scrollbar"
      "text" "xasperate" "motif" "graph" "gesture" "unistrokes" "virtual-agg"))

(defparameter Documentation-Strings
  `(("3d" "Shows how buttons pretend
to move in 3-D in the
Garnet widget set.")
    ("angle" "Shows how the angle-interactor
can rotate objects.")
    ("animator" "Shows how the animation-interactor
supports animations.")
    ("arith" "A simple visual language for arithmetic.
Demonstrates constraints, postscript and gestures.")
    ("calculator" "A simple calculator.")
    ("editor" "The editor used as a sample program in the manual.
Demonstrates the graphics selection widget.")
    ("file-browser" "Demonstrates the browser gadget using files.")
    ("gadgets" "Demonstrates some of the widgets
that have the Garnet look-and-feel.")
    ("garnetdraw" "A comprehensive drawing program.  More than a demo.
Almost a fully-functioned drawing program.
Has gridding, printing, etc.")
    ("gesture" "Demonstrates the gesture recognition interactor.")
    ("graph" "Demonstrates the automatic layout
in graphs using AggreGraphs.")
    ("grow" "Demonstrates the move-grow-interactor.")
    ("logo" "Animates the Garnet logo.")
    ("manyobjs" "A timing test that demonstrates
constraints and redrawing.")
    ("menu" "Garnet supports many
looks and feels for menus.")
    ("motif" "Demonstrates some of the widgets
that have the Motif look-and-feel.")
    ("multifont" "Demonstrates how multifont text can be used.
Includes the Lisp-mode editing.")
    ("multiwin" "Shows how Garnet interactors can be
used with multiple windows.")
    ("othello" "The game of Othello.  Demonstrates the prototype-instance
object system, and how instances are automatically
edited when prototypes are changed.")
    ("pixmap" "A simple pixmap editor demonstrates
pixmaps and virtual aggregates.")
    ("schema-browser" "Demonstrates the browser gadget showing
the hierarchy for garnet objects.")
    ("scrollbar" "Garnet supports many looks and feels for scrollbars.")
    ("text" "Demonstrates text editing.")
    ("xasperate" "A simple game.")
    ("unistrokes" "A gesture-based text editor, using a shorthand
devised by David Goldberg at Xerox PARC.")
    ("virtual-agg" "Demonstrates virtual aggregates.")
    ))

(defun Do-Go ()
  (setq *running* NIL)
  (demo-logo:do-go :dont-enter-main-event-loop T)

  (create-instance 'win1 inter:interactor-window
    (:left 0)(:top 240)(:width 270)(:height 430)
    (:title "Demos Controller")
    (:aggregate (create-instance 'agg1 opal:aggregate)))

  (create-instance 'bt garnet-gadgets:x-button-panel
    (:constant T)
    (:left 2)(:top 40)
    (:selection-function 'dispatcher)
    (:rank-margin (o-formula (ceiling (length (gvl :items)) 2)))
    (:items
        '("3d" "angle" "animator" "arith" "calculator" "editor"
          "file-browser" "gadgets" "garnetdraw" "gesture" "graph" "grow"
	  "logo" "manyobjs" "menu" "motif" "multifont" "multiwin" "othello"
	  "pixmap" "schema-browser" "scrollbar" "text" "xasperate"
	  "unistrokes" "virtual-agg")))

  ;; set up help strings for the mouseline gadget.
  (dolist (button (g-value bt :X-BUTTON-LIST :components))
    (s-value button :help-string (cadr (assoc (g-value button :string)
					      DOCUMENTATION-STRINGS
					      :test #'string=))))

  (create-instance 'qbt garnet-gadgets:text-button
    (:constant T)
    (:left 2)(:top 2)(:shadow-offset 3)
    (:font (create-instance NIL opal:font (:size :medium)(:face :bold)))
    (:string "Quit")
    (:help-string "Quits the demos-controller and all demos.")
    (:selection-function #'quit*))

  #-apple
  (create-instance 'demos-mouseline gg:mouselinepopup
    #+garnet-processes (:wait-amount 2)
    #-garnet-processes (:wait-amount NIL))

  (opal:add-components agg1 bt qbt #-apple demos-mouseline)

  (create-instance 'win2 garnet-gadgets:scrolling-window-with-bars
    (:constant T :except :top :left :width :height :title :total-height)
    #-apple(:left 0) #-apple(:top 720)
    #+apple(:left 300) #+apple(:top 50)
    (:width 700)(:height 180)
    (:title "Instructions for Demos")
    (:h-scroll-bar-p NIL)
    (:total-width 700)
    (:total-height (o-formula (+ 5 (gvl :inner-aggregate :height)) 200)))

  (opal:update win1)
  (opal:update win2)

  (create-instance 'text opal:multifont-text
    (:left 5)(:top 5)
    (:initial-text "Hold mouse over a button for 2 seconds to see
a description of the demo.  This demonstrates the mouseline gadget.

Click the button to start the demo."))

  (opal:add-components (g-value win2 :inner-aggregate) text)

  (opal:update win2)
  ;;if not CMU CommonLisp, then start the main event loop to look for events
  #-cmu (inter:main-event-loop)
)

(defun Do-Stop ()
    (dolist (item *running*)
      (unless (string= item "logo")
        (funcall (intern "DO-STOP"
            (cadar (member item *package-list* :key #'car :test #'string=))))))
    (demo-logo:do-stop)
    (opal:destroy win1)
    (opal:destroy win2)
    (setq win1 NIL)
  ;;if not CMU CommonLisp, then exit the main event loop
    #-cmu (inter:exit-main-event-loop)
)

(defun quit* (inter obj)
    (declare (ignore inter obj))
    (do-stop))

(defun start (objlist)
    (when (string= (car objlist) "logo") 
	  (opal:set-text text " ")
	  (opal:update win2)
	  (demo-logo:re-animate)
	  (return-from start))
    (opal:set-text text
		   (list ""
			 (list (cons "Please wait... Loading."
				     (opal:get-standard-font
				      :fixed :bold-italic :very-large)))))
    (opal:update win2)
    (let ((kr::*warning-on-create-schema* nil)
	  (package-name (cadar (member (car objlist) *package-list* :key #'car
				       :test #'string=))))

      (when (member (car objlist) *unloaded* :test #'string=)
            (load 
              (user::garnet-pathnames
	       (string-downcase package-name) user::Garnet-Demos-PathName))
            (setq *unloaded* (remove (car objlist) *unloaded* :test #'string=)))
      (opal:set-text text (string-trim (list #\newline #\space)
       (with-output-to-string (*standard-output*)
        (funcall
         (intern "DO-GO"
          package-name)
         :dont-enter-main-event-loop T
	 :double-buffered-p T)))))
    (garnet-gadgets:scroll-win-to win2 0 0)
    (opal:update win2))

(defun deselected (objlist)
    (dolist (item *running*)
        (unless (member item objlist :test #'string=)
            (return-from deselected item))))

(defun stop (item)
    (opal:set-text text "")
    (opal:update win2)
    (when (string= item "logo") (return-from stop))
    (funcall (intern "DO-STOP"
        (cadar (member item *package-list* :key #'car :test #'string=)))))

(defun dispatcher (inter obj)
    (declare (ignore inter obj))
    (let ((objlist (g-value bt :value)))
      (if (> (length objlist) (length *running*))
        (start objlist)
        (stop (deselected objlist)))
      (setq *running* (copy-list objlist))))

(defun user::Garnet-Note-Quitted (package)
  (let ((button-name NIL))
  (when (and (boundp 'win1) win1)
    (dolist (item *package-list*)
      (when (string= package (symbol-name (cadr item)))
          (setq button-name (car item))
          (return)))
    (s-value bt :value (remove button-name (g-value bt :value) :test #'string=))
    (setq *running* (copy-list (g-value bt :value)))
    (opal:update win1)
    T)))

