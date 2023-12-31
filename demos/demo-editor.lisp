;;; @i[-*- Mode: LISP; Syntax: Common-Lisp; Package: DEMO-EDITOR; Base: 10 -*-]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @i[        The Garnet User Interface Development Environment.      ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @i[This code was written as part of the Garnet project at          ]
;;; @i[Carnegie Mellon University, and has been placed in the public   ]
;;; @i[domain.  If you are using this code or any part of Garnet,      ]
;;; @i[please contact garnet@cs.cmu.edu to be put on the mailing list. ]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;@i{This file is a sample of a graphics editor created with Garnet.  It is}
;;;@i{designed to be a model for other code development, and therefore uses}
;;;@i{all the most up-to-date Garnet features.}
;;;
;;;@i{** Call (demo-editor:Do-Go) to start and (demo-editor:Do-Stop) to stop **}
;;;
;;;@i{Designed and implemented by Brad A. Myers}

#|
==================================================================
Change log:
         05/22/94 Marty Geier - Made main window more viewable
         04/07/92 Ed Pervin - Move in-package to beginning of file.
	 02/14/92 Ed Pervin - Converted formulas to o-formulas.
         11/07/90 Ed Pervin - In Do-Quit, destroy the window BEFORE exit-main-event-loop.
         09/04/90 Osamu Hashimoto - Added Garnet-Note-Quitted for demo-controller,
                                    Changed window positon
         01/04/90 Andrew Mickish - Added auto-loading of "text-buttons-loader",
                  "graphics-loader", and "arrow-line-loader"
===================================================================
|#

(in-package :DEMO-EDITOR)

;;;  Load text-buttons-loader, graphics-loader, and arrow-line-loader unless
;;;  already loaded
;;;
(dolist (pair '((:text-buttons "text-buttons-loader")
		(:graphics-selection "graphics-loader")
		(:arrow-line "arrow-line-loader")))
  (when (not (get :garnet-modules (car pair)))
    (user::garnet-load (concatenate 'string "gadgets:" (cadr pair)))))


;; Eliminate compile warnings for named objects
;;
(declaim (special MYARROWLINE MYLABELEDBOX))


;;;@newpage()
;;;------------------------------------------------------------------------
;;;@i{First create the prototypes for the box and lines}
;;;------------------------------------------------------------------------


(create-instance 'myarrowline garnet-gadgets:arrow-line
		 (:from-obj NIL) ;@i{set this with the object this arrow is from}
		 (:to-obj NIL)   ;@i{set this with the object this arrow is from}
		 (:x1 (o-formula (opal:gv-center-x (gvl :from-obj))))
		 (:y1 (o-formula (opal:gv-center-y (gvl :from-obj))))
		 (:x2 (o-formula (opal:gv-center-x (gvl :to-obj))))
		 (:y2 (o-formula (opal:gv-center-y (gvl :to-obj))))
		 (:open-p NIL)
		 (:visible (o-formula (and (gvl :from-obj)(gvl :to-obj))))
		 (:line-p T) ;@i{so that the selection object will know what kind this is}
		 )

(create-instance 'mylabeledbox opal:aggregadget
		 (:box (list 20 20 40 20)) ;@i{this will be set by the}
					   ;@i{interactors with the size of this box.}

		 (:lines-at-this-box NIL)  ;@i{Keep track of lines pointing}
					   ;@i{to me, in case I am deleted.}

		 ;;@i{Set up a circular constraint between this string slot and the}
		 ;;@i{string slot in the label.  If either is changed, the other is}
		 ;;@i{automatically updated.  For circular constraints, it is}
		 ;;@i{important to have an initial value, here it is the empty string.}
		 (:string (o-formula (gvl :label :string) "")) 

		 (:line-p NIL) ;@i{so that the selection object will know what kind this is}
		 (:parts
		  `((:frame ,opal:roundtangle
			(:radius 15)
			(:left ,(o-formula (first (gvl :parent :box))))
			(:top ,(o-formula (second (gvl :parent :box))))
			(:width ,(o-formula (third (gvl :parent :box))))
			(:height ,(o-formula (fourth (gvl :parent :box)))))
		    (:label ,opal:cursor-text
			(:string ,(o-formula (gvl :parent :string) ""))
			(:cursor-index NIL)
			;;@i{center me horizontally with respect to the frame}
			(:left ,(o-formula
				 (- (opal:gv-center-x (gvl :parent :frame))
				    (floor (gvl :width) 2))))
			(:top ,(o-formula
				(+ (gvl :parent :frame :top) 5)))))))

;;;@newpage()
;;;------------------------------------------------------------------------
;;;@i{Create main menu object}
;;;------------------------------------------------------------------------

;;@i{Create an arrow and a box menu object, and put them in a menu, with an}
;;@i{interactor and feedback object to show which is selected.  }
;;@i{Agg is the top level aggregate to put the menu in, and window is the window.}
;;@i{The :line-p slot of the agg is set with a formula to tell whether in line mode}
;;@i{or not.}
(defun create-mode-menu (agg window)
  (let (feedback boxitem arrowitem)
    (setf boxitem (create-instance NIL mylabeledbox
				   (:box (list 20 20 80 40))
				   (:string "Label")))
    ;;@i{the arrow will be inside a box.}
    (setf arrowitem
	  (create-instance NIL opal:aggregadget
		(:parts
		 `((:frame ,opal:rectangle
		       (:left 20)(:top 80)(:width 80)(:height 40))
		   (:line ,garnet-gadgets:arrow-line
		       (:open-p NIL)
		       (:x1 ,(o-formula (+ (gvl :parent :frame :left) 2)))
		       (:y1 ,(o-formula
			      (opal:gv-center-y (gvl :parent :frame))))
		       (:x2 ,(o-formula (+ (gvl :parent :frame :left) 76)))
		       (:y2 ,(o-formula (gvl :y1))))))))
				     
    ;;@i{The interactor (defined below) will set the :selected slot of the aggregate.}
    ;;@i{Use this to determine where the feedback should be.}
    ;;@i{We need to use formula rather than o-formula here so we can have a direct}
    ;;@i{reference to agg (use formula whenever you need to reference an object that}
    ;;@i{is not stored in a slot of the current object).  Notice the use of}
    ;;@i{back-quote and comma to get a reference to the agg object.}
    (setf feedback (create-instance NIL opal:rectangle
			    (:line-style opal:line-4)
			    (:filling-style NIL)
			    (:left (o-formula (- (gvl :parent :selected :left) 6)))
			    (:top (o-formula (- (gvl :parent :selected :top) 6)))
			    (:width (o-formula
				      (+ (gvl :parent :selected :width) 12)))
			    (:height (o-formula
				       (+ (gvl :parent :selected :height) 12)))
			    (:visible (o-formula (gvl :parent :selected)))
			    (:draw-function :xor)
			    (:fast-redraw-p T)))
    (opal:add-components agg boxitem arrowitem feedback)

    ;;@i{use the :menuobjs slot to hold the items that can be selected}
    (s-value agg :menuobjs (list boxitem arrowitem))

    ;;@i{default mode is the rectangle}
    (s-value agg :selected boxitem)

    ;;@i{The :line-p slot of the agg is set with a formula to tell whether in line mode or not.}
    ;;	                          Remember, arrowitem = (second (gvl :menuobjs))
    (s-value agg :line-p (o-formula (eq (gvl :selected) (second (gvl :menuobjs)))))

    ;;@i{now create an interactor to choose which mode}
    (create-instance NIL inter:menu-interactor
		     (:window window)
		     (:start-event '(:leftdown :rightdown)) ;@i{either one}
		     (:start-where `(:list-element-of ,agg :menuobjs)))))


;;;@newpage()
;;@i{This creates the menu of commands.  For now, it only has "delete" and "quit" in it.}
;;@i{The menu is stored into the aggregate agg.  Returns the menu created.}
(defun create-menu (agg)
  (let ((menu (create-instance NIL Garnet-gadgets:Text-Button-Panel
			(:constant T)
			(:items '(("Delete" Delete-Object) ("Quit" Do-Quit)))
			(:left 20)
			(:top 200)
			(:font Opal:Default-font)
			(:shadow-offset 5)
			(:final-feedback-p NIL))))
    (opal:add-components agg menu)
    menu))
				 
;;;********************************************************************
;;;@i{Procedures to do the work}
;;;********************************************************************

;;;@i{Delete-Line is called from delete object to delete lines}
(defun Delete-Line(line-obj)
  (let ((from-obj (g-value line-obj :from-obj))
	(to-obj (g-value line-obj :to-obj)))
    ;;@i{remove this line from the boxes' lists}
    (s-value from-obj :lines-at-this-box
	     (delete line-obj (g-value from-obj :lines-at-this-box)))
    (s-value to-obj :lines-at-this-box
	     (delete line-obj (g-value to-obj :lines-at-this-box)))
    (opal:destroy line-obj)))
  
;;;@i{Delete-object is called from the main menu routine}
(defun Delete-Object (toolkit-obj menu-item)
  (declare (ignore menu-item))
  (let ((selected-obj (g-value toolkit-obj :selection-obj :value)))
    (if selected-obj
      (progn
	;;@i{first turn off selection}
	(s-value (g-value toolkit-obj :selection-obj) :value NIL)
	;;@i{now delete object}
	(if (g-value selected-obj :line-p)
	    ;;@i{then deleting a line}
	    (Delete-Line selected-obj)
	    ;;@i{else deleting a box}
	    (progn
	      ;;@i{first delete all lines to this box}
	      (dolist (line-at-box (g-value selected-obj :lines-at-this-box))
		(delete-line line-at-box))
	      ;;@i{now delete the box}
	      (opal:destroy selected-obj))))
	;;@i{else nothing selected}
	(inter:beep))))

(defun Do-Quit (toolkit-obj menu-item)
  (declare (ignore menu-item))
  (opal:destroy (g-value toolkit-obj :window))
  ;;for demo-controller
  (unless (and (fboundp 'User::Garnet-Note-Quitted)
	       (User::Garnet-Note-Quitted "DEMO-EDITOR")))
)

;;;@newpage()
;;;@i{Create a new object.  Get the type of object to create from the interactor.}
;;;@i{This procedure is called as the final-function of the two-point interactor.}
(defun Create-New-Obj (inter point-list)
  (let ((agg (g-value inter :objs-aggregate))
	(line-p (g-value inter :line-p))) ;@i{create a line or rectangle}

    (if line-p
	;;@i{then create a line, first have to find the objects where the line is drawn}
	(let ((from-box (opal:point-to-component agg (first point-list)
				      (second point-list) :type mylabeledbox))
	      (to-box (opal:point-to-component agg (third point-list)
				      (fourth point-list) :type mylabeledbox))
	      new-line)
	  ;;@i{If one end of the arrow is not inside a box, or is from and to the same box, then beep.}
	  (if (or (null from-box)(null to-box) (eq from-box to-box))
	      (inter:beep)
	      ;;@i{ else draw the arrow.}
	      (progn
		(setf new-line (create-instance NIL myarrowline
						(:from-obj from-box)
						(:to-obj to-box)))
		;;@i{keep track in case boxes are deleted so can delete this line.}
		(push new-line (g-value from-box :lines-at-this-box))
		(push new-line (g-value to-box :lines-at-this-box))
		
		(opal:add-component agg new-line))))
	;;@i{else, create a new box}
	(let ((textinter (g-value inter :textinter))
	      (new-box (create-instance NIL mylabeledbox 
			   (:box (copy-list point-list))))) ;@i{have to make}
							    ;@i{a copy of list since}
							    ;@i{the interactor}
							    ;@i{re-uses the same list}
	  (opal:add-component agg new-box)
	  ;;@i{now start the interactor to allow the user to type the label.}
	  ;;@i{Obj-to-change is the label object of the new box.}
	  (s-value textinter :obj-to-change (g-value new-box :label))
	  (inter:start-interactor textinter)))))

;;;@newpage()
;;;********************************************************************
;;;@i{Main procedures}
;;;********************************************************************

(defparameter current-window NIL) ;@i{this global variable is only used for}
				  ;@i{ the debugging function below: do-stop}

(defun Do-Go (&key dont-enter-main-event-loop double-buffered-p)
  (let (top-win work-win top-agg work-agg selection objs-agg menu edit-text)
    ;;;@i{create top-level window}
    (setf top-win (create-instance NIL inter:interactor-window
                     (:left 20) (:top 45)
                     (:double-buffered-p double-buffered-p)
		     (:width 700) (:height 400)(:title "GARNET Sample Editor")
		     (:icon-title "Graphics Editor")))
    (setf current-window top-win)

    ;;;@i{create window for the work area}
    (setf work-win (create-instance NIL inter:interactor-window
		     (:left 150)
		     (:top -2) ;@i{no extra border at the top}
		     (:width (o-formula (- (gvl :parent :width) 150)))
		     (:height (o-formula (gvl :parent :height)))
		     (:double-buffered-p double-buffered-p)
		     (:border-width 2)
		     (:parent top-win)))

    ;;;@i{create the top level aggregate in the windows}
    (setq top-agg (create-instance NIL opal:aggregate
		     (:left 0)(:top 0)
		     (:width (o-formula (gvl :window :width)))
		     (:height (o-formula (gvl :window :height)))))

    (setq work-agg (create-instance NIL opal:aggregate
		     (:left 0)(:top 0)
		     (:width (o-formula (gvl :window :width)))
		     (:height (o-formula (gvl :window :height)))))
    ;;;@i{create an aggregate to hold the user-created objects}
    (setq objs-agg (create-instance NIL opal:aggregate
		     (:left 0)(:top 0)
		     (:width (o-formula (gvl :window :width)))
		     (:height (o-formula (gvl :window :height)))))
    (opal:add-component work-agg objs-agg)

    ;;;@i{create menus}
    (create-mode-menu top-agg top-win)
    (setf menu (create-menu top-agg))

    ;;;@i{create a graphics selection object}
    (setq selection (create-instance NIL Garnet-Gadgets:graphics-selection
			(:start-where (list :element-of-or-none objs-agg))
			(:movegrow-lines-p NIL) ;@i{can't move lines}
			;;@i{move objects while cursor in the work window}
			(:running-where (list :in work-win))))
    (opal:add-component work-agg selection)
				     
    ;;@i{store the selection object in a new slot of the menu so that the delete}
    ;;@i{function can find which object is selected.}
    (s-value menu :selection-obj selection)

    ;;;@i{Create an interactor to edit the text of the labels when they are first}
    ;;;@i{created.  This interactor will never start by itself, but is started}
    ;;;@i{explicitly using Inter:Start-Interactor in the Create-New-Object function.}
    (setf edit-text (create-instance NIL Inter:Text-Interactor
		       (:obj-to-change NIL) ;@i{this is set when the interactor is started}
		       (:start-event NIL) ;@i{won't start by itself}
		       (:start-where NIL) ;@i{won't start by itself}
		       (:stop-event '(#\return :any-mousedown)) ;@i{either stops it}
		       (:window work-win)))
    ;;@i{cont., next page}@newpage()
    ;;@i{The next interactor edits the text when the user presses on a string.}
    (create-instance NIL Inter:Text-Interactor
		       (:stop-event '(#\return :any-mousedown)) ;@i{either stops it}
		       (:start-where (list :leaf-element-of objs-agg
					   :type Opal:cursor-text))
		       ;;@i{high priority so that if this one runs, the object}
		       ;;@i{underneath will not become selected.}
		       (:waiting-priority inter:high-priority-level)
		       (:window work-win))

    ;;;@i{create an interactor to create the new objects}
    (create-instance NIL Inter:Two-Point-Interactor
		     (:start-event :rightdown)
		     (:start-where T)
		     (:running-where (list :in work-win))
		     (:window work-win)
		     (:abort-event '(:control-g :control-\g))
		     (:line-p (o-formula (gvl :window :parent :aggregate :line-p)))
		     ;@i{The next 2 slots are used by the Create-New-Obj procedure,}
		     ;@i{not by this interactor itself.}
		     (:objs-aggregate objs-agg)
		     (:textinter edit-text)
		     (:selection selection)

		     (:feedback-obj
		      ;;@i{use the feedback objects in the graphics-selection object}
		      ;;@i{pick which feedback depending on whether drawing line or box}
                      (o-formula
                       	(if (gvl :line-p)
                            (gvl :selection :line-movegrow-feedback)
                            (gvl :selection :rect-movegrow-feedback))))
	     (:final-function #'Create-New-Obj))

    ;;;@i{Now, add the aggregates to the window and update}
    (s-value top-win :aggregate top-agg)
    (s-value work-win :aggregate work-agg)
    (opal:update top-win)  ;;@i{will also update work-win}

  ;;@i{** Do-Go **}
  (Format T "~%Demo-Editor: 
  Press with left button on top menu to change modes (box or line).
  Press with left button on bottom menu to execute a command.
  Press with right button in work window to create a new object
        of the current mode.
  Boxes can be created anywhere, but lines must start and stop inside boxes.
  After creating a box, you should type the new label.
  Press with left button on text string to start editing that string.
        While editing a string, type RETURN or press a mouse button to stop.
  Press with left button in work window to select an object.
  Press with left button on white selection square to move an object.
  Press with left button on black selection square to change object size.
  While creating, moving, or growing a box, move outside window and release or
        hit ^G or ^g to abort.
  ~%")

  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))

    ;;@i{return top window}
    top-win))

;;@i{** This is mainly for debugging, since usually the quit button in the menu will be used.}
(defun Do-Stop ()
  (opal:destroy current-window))

