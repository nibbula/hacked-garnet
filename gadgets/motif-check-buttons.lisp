;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-GADGETS; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Motif-Check-buttons
;;;
;;;  Features and operation of check-buttons:
;;;     1)  Click the left mouse button in a check button to select it.
;;;     2)  Pressing the space-bar selects the currently outlined button,
;;;         and Tab toggles between gadgets in the demo.
;;;     3)  The top level :value slot points to the string of the currently
;;;         selected button.  This slot can be set with S-VALUE.
;;;     4)  The top level :value-obj slot points to the currently selected
;;;         button, and can be set directly with S-VALUE to select a button.
;;;     5)  The :items slot may contain functions to be executed as each
;;;         button is selected, and :selection-function may contain a
;;;         function to be executed when any button becomes selected.
;;;
;;;  Customizable slots:
;;;     1)  All customizable slots of an aggrelist:
;;;            Direction -- :vertical or :horizontal
;;;            V-spacing -- distance between buttons, if vertical orientation
;;;            H-spacing -- same, if horizontal orientation
;;;            Fixed-width-p -- whether all the buttons should be the width of
;;;                             :fixed-width-size (default is T)
;;;            Fixed-height-p -- same, but with heights
;;;            Fixed-width-size -- width of all components (default is the
;;;                                width of the widest button)
;;;            Fixed-height-size -- same, but with heights 
;;;            H-align -- how to align text within buttons horizontally
;;;                       :left, :center, or :right (default is :center)
;;;            V-align -- how to align text within buttons, vertically
;;;                       :top, :bottom, or :center (default is :center)
;;;            Rank-margin -- after this many components, a new row (or column)
;;;                           will be started
;;;            Pixel-margin -- absolute position in pixels after which a new
;;;                            row (or column) will be started
;;;            Indent -- amount to indent the new row (or column) in pixels
;;;     2)  Left, top
;;;     3)  Button-width -- The width of the square button object
;;;     4)  Text-offset -- the distance from the edge of the longest text to
;;;                        the frame of the button
;;;     5)  Text-on-left-p -- Determines on which side of the buttons the text
;;;                           should be placed.
;;;     6)  Keyboard-selection-p -- Whether the keyboard interactor should
;;;            operate on the button (or button panel)
;;;     7)  Font -- the font in which the text will appear
;;;     8)  String (in single buttons)
;;;     9)  Active-p (in single buttons) -- Whether the single button can be
;;;                                         selected.
;;;    10)  Items (in button panels) -- This can be: 
;;;                  A list of strings, as in '("Large" ...), or
;;;                  a list of atoms, as in '(:center ...), or
;;;                  a list of string/function pairs, '(("Cut" Cut-FN) ...), or
;;;                  a list of atom/function pairs, '((:center Center-FN) ...).
;;;                  Each function will be executed when the associated button
;;;                  becomes selected.  The parameters are the top-level
;;;                  GADGET and the ITEM-STRING.
;;;    11)  Inactive-items (in button panels) -- A list of strings designating
;;;                  items that are not selectable.
;;;    12)  Final-feedback-p -- whether to leave the currently selected button
;;;                             depressed.
;;;    13)  Foreground-color
;;;    14)  Selection-function -- Global function to be executed when any
;;;                               button is selected.  Parameters are the
;;;                               top-level GADGET and the ITEM-STRING.
;;;
;;;  NOTE:  This module requires several schemata defined in motif-parts.
;;;         Thus, motif-parts.fasl must be loaded before this module.
;;;
;;;  Motif-Check-Buttons Demo:
;;;     The function below creates window containing a motif-check-button and
;;;     a motif-check-button-panel.
;;;     To run it, enter (GARNET-GADGETS:motif-check-buttons-go).
;;;     To stop, enter (GARNET-GADGETS:motif-check-buttons-stop).
;;;
;;;  Written by Andrew Mickish

;;; CHANGE LOG:
;;; 07/14/93  Andrew Mickish - Added :keyboard-selection-p dependency in
;;;             :keyboard-selection formula
;;; 05/26/93  Andrew Mickish - Fixed constant declarations for new aggrelists
;;; 05/13/93  Andrew Mickish - :prev-visible ---> :prev
;;; 02/23/93  Andrew Mickish - Added :string-set-func
;;; 02/10/93  Andrew Mickish - Made :items and :inactive-items items-type
;;; 01/25/93  Andrew Mickish - Added dependency on :items list so that
;;;             the aggrelist's :fix-update-slots method will be invoked
;;; 12/15/92  Andrew Mickish - Added type and parameter declarations
;;; 11/20/92  Andrew Mickish - Added :fixed-width, :fixed-height to button-list;;; 06/16/92  Andrew Mickish - Added objects in :items list
;;; 02/28/92  Andrew Mickish - Added :style-array references
;;; 02/27/92  Andrew Mickish - Removed :leftdown case from panel's :KEY inter
;;; 02/11/92  Andrew Mickish - Added :maybe-constant list
;;; 10/13/91  Andrew Mickish - Added :leftdown case in the panel's :KEY
;;;             interactor so the keyboard selection will follow mouse clicks
;;; 07/26/91  Andrew Mickish - Added :toggle-p
;;; 05/14/91  Andrew Mickish - Fixed :selected slot of :button-list
;;; 05/13/91  Edward Pervin - Button-width was not being used in 
;;;		the formula for :button-left in motif-check-button.
;;; 03/01/91  Andrew Mickish - Created

(in-package "GARNET-GADGETS")

;; (eval-when (eval load compile)
;;   (export '(Motif-Check-Button Motif-Check-Button-Panel))
;;   #+garnet-debug
;;   (export '(Motif-Check-Buttons-Go Motif-Check-Buttons-Stop
;; 	    Demo-Motif-Check-Button Demo-Motif-Check-Button-Panel
;; 	    Motif-Check-Buttons-Top-Agg Motif-Check-Buttons-Win
;; 	    Demo-Motif-Check-Button2)))


(create-instance 'MOTIF-CHECK-BUTTON-TEXT-LABEL-PROTOTYPE opal:text
  (:constant '(:actual-heightp))
  (:left (o-formula (gvl :parent :text-left)))
  (:top (o-formula (- (gv (kr-path 0 :parent) :center-y)
		      (floor (gvl :height) 2))))
  (:string (o-formula (gv (kr-path 0 :parent) :string)))
  (:font (o-formula (gv (kr-path 0 :parent) :font)))
  (:line-style (o-formula
		(let ((p (kr-path 0 :parent)))
		  (if (gv p :active-p)
		      opal:default-line-style
		      (gv p :stippled-line-style))))))


(create-instance 'MOTIF-CHECK-BUTTON MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :button-width :text-offset :text-on-left-p
			 :active-p :string :font :toggle-p :foreground-color
			 :keyboard-selection-p :value :selection-function
			 :visible)
	    (:type ((integer 0) :button-width)
		   (integer :text-offset)
		   (kr-boolean :text-on-left-p :active-p :toggle-p
		    :keyboard-selection-p)
		   ((or string keyword (satisfies schema-p)) :string)
		   ((or null string keyword (satisfies schema-p)) :value)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((is-a-p opal:color) :foreground-color)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :button-width :text-offset :active-p
			      :text-on-left-p :toggle-p :string :font
			      :foreground-color :visible))
   (:left 0)(:top 0)
   (:button-width 12)
   (:text-offset 5)
   (:text-on-left-p NIL)
   (:active-p T)
   (:toggle-p T)
   (:string "Motif Check Button")
   (:font opal:default-font)
   (:keyboard-selection-p NIL)
   (:foreground-color opal:MOTIF-GRAY)
   (:selection-function NIL)

   (:background-fill (o-formula (if (gv opal:color :color-p)
				    (aref (gvl :style-array)
					  *background-fill-index*)
				    opal:black-fill)))
   (:text-left (o-formula (let ((left-p (gvl :text-on-left-p))
				(left (gvl :left))
				(button-width (gvl :button-width))
				(text-offset (gvl :text-offset)))
			    (+ 3 (if left-p
				     left
				     (+ left button-width
					text-offset))))))
   (:text-width (o-formula (gvl :text :width)))
   (:center-y (o-formula (+ 2 (gvl :top)
			    (floor (MAX (gvl :button-width)
					(gvl :text :height)) 2))))
   (:button-left (o-formula (let ((left-p (gvl :text-on-left-p))
				  (left (gvl :left))
				  (text-offset (gvl :text-offset))
				  (text-width (gvl :text-width)))
			      (+ 3 (if left-p
				       (+ left text-width text-offset)
				       left)))))
   (:button-top (o-formula (- (gvl :center-y) (floor (gvl :button-width) 2))))
   (:button-bottom (o-formula (+ (gvl :center-y)
				 (floor (gvl :button-width) 2))))

   (:width (o-formula (+ 8 (gvl :text-width) (gvl :text-offset)
			 (gvl :button-width))))
   (:height (o-formula (+ 4 (MAX (gvl :button-width) (gvl :text :height)))))

   (:interim-selected NIL)
   (:depressed-p (o-formula (or (gvl :interim-selected) (gvl :selected))))
   (:value (o-formula (if (gvl :selected) (gvl :string))))
   (:selected (o-formula (gvl :value)))
   (:text-label-prototype MOTIF-CHECK-BUTTON-TEXT-LABEL-PROTOTYPE)
   (:parts
    `((:BUTTON ,MOTIF-BOX
	       (:left ,(o-formula (gv (kr-path 0 :parent) :button-left)))
	       (:top ,(o-formula (- (gv (kr-path 0 :parent) :center-y)
				    (floor (gvl :height) 2))))
	       (:width ,(o-formula (gv (kr-path 0 :parent) :button-width)))
	       (:height ,(o-formula (gv (kr-path 0 :parent) :button-width)))
	       (:depressed-p ,(o-formula (gv (kr-path 0 :parent) :depressed-p))))
      (:TEXT ,#'opal::Single-Button-Get-Label)
      (:SEL-BOX ,MOTIF-SELECTION-BOX
                (:obj-over ,(o-formula (gvl :parent))))))
   (:interactors
    `((:PRESS ,MOTIF-SINGLE-PRESS)
      (:KEY ,MOTIF-SINGLE-KEY))))


(create-instance 'MOTIF-CHECK-BUTTON-PANEL MOTIF-GADGET-PROTOTYPE
  :declare ((:parameters :left :top :button-width :text-offset :text-on-left-p
			 :items :inactive-items :keyboard-selection-p :value
			 :keyboard-selection :font :foreground-color :active-p
			 :direction :v-spacing :h-spacing :fixed-width-p
			 :fixed-height-p :fixed-width-size :fixed-height-size
			 :h-align :rank-margin :pixel-margin :indent
			 :selection-function :visible)
	    (:type ((integer 0) :button-width)
		   (integer :v-spacing :h-spacing :text-offset :indent)
		   ((or null (integer 0)) :fixed-width-size :fixed-height-size
		    :rank-margin :pixel-margin)
		   (kr-boolean :text-on-left-p :keyboard-selection-p
		    :active-p :fixed-width-p :fixed-height-p)
		   (items-type :items :inactive-items)
		   (list :value :value-obj)
		   ((or null string keyword (satisfies schema-p))
		    :keyboard-selection)
		   ((or null (satisfies schema-p)) :keyboard-selection-obj)
		   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((is-a-p opal:color) :foreground-color)
		   ((member :vertical :horizontal) :direction)
		   ((member :left :center :right) :h-align)
		   ((or null function symbol) :selection-function))
	    (:maybe-constant :left :top :button-width :text-offset
			     :text-on-left-p :items :font :foreground-color
			     :direction :v-spacing :h-spacing :v-align :h-align
			     :indent :fixed-width-p :fixed-width-size
			     :fixed-height-p :fixed-height-size :rank-margin
			     :pixel-margin :inactive-items :active-p :visible))

   (:left 0) (:top 0)
   (:button-width 12)
   (:text-offset 5)
   (:text-on-left-p NIL)

   (:items '("Check 1" "Check 2" "Check 3"))
   (:inactive-items NIL)
   (:keyboard-selection-p NIL)
   (:keyboard-selection (o-formula
			 (progn
			   ;; Set up dependency, so won't become constant
			   (gvl :keyboard-selection-p)
			   (first
			    (last (set-exclusive-or (gvl :item-objs)
						    (gvl :inactive-items)
						    :test #'equal))))))
   (:keyboard-selection-obj (o-formula
			     (let ((rank (position (gvl :keyboard-selection)
						   (gvl :item-objs)
						   :test #'equal)))
			       (nth rank (gvl :button-list :components)))))
   (:font opal:default-font)
   (:foreground-color opal:MOTIF-GRAY)
   (:direction :vertical)
   (:v-spacing 5) (:h-spacing 5)
   (:v-align :top)
   (:h-align (o-formula (if (gvl :text-on-left-p) :right :left)))
   (:indent 0) (:rank-margin NIL) (:pixel-margin NIL)
   (:fixed-width-p T)
   (:fixed-width-size (o-formula (+ 8 (gvl :button-list :tail
					   :max-text-width-thus-far)
				    (gvl :text-offset)
				    (gvl :button-width))))
   (:fixed-height-p NIL)
   (:fixed-height-size (o-formula (+ 4 (MAX (gvl :button-list :tail
						 :max-text-height-thus-far)
					    (gvl :button-width)))))
   (:selection-function NIL)
   (:background-fill (o-formula (if (gv opal:color :color-p)
				    (aref (gvl :style-array)
					  *background-fill-index*)
				    opal:black-fill)))
   (:width (o-formula (gvl :button-list :width)))
   (:height (o-formula (gvl :button-list :height)))

   (:value-obj NIL)
   (:value (o-formula (let ((obj-list (gv-local :self :value-obj)))
			(if obj-list
			    (mapcar #'(lambda (object)
					(g-value object :item-obj))
				    obj-list)))))

   (:item-objs (o-formula (let ((items (gvl :items)))
			    (if (listp (first items))
				(mapcar #'car items)
				items))))
   (:strings (o-formula (mapcar #'(lambda (item-obj)
				    (if (stringp item-obj)
					item-obj
					(if (schema-p item-obj)
					    item-obj
					    ;; Must be an atom
					    (string-capitalize
					     (string-trim ":" item-obj)))))
				(gvl :item-objs))))
   (:actions (o-formula (let ((items (gvl :items)))
			  (when (listp (first items))
			    (mapcar #'cadr items)))))
   (:active-p (o-formula (> (length (gvl :items))
			    (length (gvl :inactive-items)))))
   (:text-label-prototype MOTIF-CHECK-BUTTON-TEXT-LABEL-PROTOTYPE)
   (:parts
    `((:BUTTON-LIST ,opal:aggrelist
       (:constant (:fixed-width-p :fixed-height-p :fixed-width-size
		   :fixed-height-size :h-align :v-align))
       (:left ,(o-formula (gv (kr-path 0 :parent) :left)))
       (:top ,(o-formula (gv (kr-path 0 :parent) :top)))
       (:direction ,(o-formula (gvl :parent :direction)))
       (:v-spacing ,(o-formula (gv (kr-path 0 :parent) :v-spacing)))
       (:h-spacing ,(o-formula (gv (kr-path 0 :parent) :h-spacing)))
       (:fixed-width-p NIL) (:fixed-height-p NIL)
       (:rank-margin ,(o-formula (gv (kr-path 0 :parent) :rank-margin)))
       (:pixel-margin ,(o-formula (gv (kr-path 0 :parent) :pixel-margin)))
       (:indent ,(o-formula (gv (kr-path 0 :parent) :indent)))
       (:items ,(o-formula (gv (kr-path 0 :parent) :items)))
       (:selected
	,(o-formula
	  (let* ((p (kr-path 0 :parent))
		 (value-list (gv p :value))
		 (components (gvl :components)))
	    (if value-list
		(let ((index-list
		       (remove NIL
			(mapcar #'(lambda (item-obj)
				    (position item-obj (gv p :item-objs)
					      :test #'equal))
				value-list))))
		  (mapcar #'(lambda (index) (nth index components))
			  index-list))))))
       (:item-prototype
	(,MOTIF-CHECK-BUTTON
	 (:max-text-width-thus-far
	  ,(o-formula (if (gvl :prev)
			  (MAX (gvl :prev :max-text-width-thus-far)
			       (gvl :text :width))
			  (gvl :text :width))))
	 (:max-text-height-thus-far
	  ,(o-formula (if (gvl :prev)
			  (MAX (gvl :prev :max-text-height-thus-far)
			       (gvl :text :height))
			  (gvl :text :height))))
	 (:button-width ,(o-formula (gv (kr-path 0 :parent :parent)
					:button-width)))
	 (:selected ,(o-formula (member (gv :self)
					(gv (kr-path 0 :parent) :selected))))
	 (:active-p ,(o-formula
		      (and (not (member (gvl :item-obj)
					(gv (kr-path 0 :parent :parent)
					    :inactive-items)
					:test #'equal))
			   (gv (kr-path 0 :parent :parent) :active-p))))
	 (:foreground-fill ,(o-formula (gv (kr-path 0 :parent :parent)
					   :foreground-fill)))
	 (:background-fill ,(o-formula (gv (kr-path 0 :parent :parent)
					   :background-fill)))
	 (:highlight-fill ,(o-formula (gv (kr-path 0 :parent :parent)
					  :highlight-fill)))
	 (:shadow-fill ,(o-formula (gv (kr-path 0 :parent :parent)
				       :shadow-fill)))
	 (:stippled-line-style ,(o-formula (gv (kr-path 0 :parent :parent)
					       :stippled-line-style)))
	 (:keyboard-selection-p ,(o-formula (gv (kr-path 0 :parent :parent)
						:keyboard-selection-p)))
	 (:text-on-left-p ,(o-formula (gv (kr-path 0 :parent :parent)
					  :text-on-left-p)))
	 (:text-offset ,(o-formula (gv (kr-path 0 :parent :parent) :text-offset)))
	 (:text-left ,(o-formula
		       (let* ((p (kr-path 0 :parent :parent))
			      (base-left (+ 3 (gvl :left)
					    (if (gv p :text-on-left-p)
						0
						(+ (gvl :button-width)
						   (gvl :text-offset))))))
			 (case (gv p :h-align)
			   (:left base-left)
			   (:center (+ base-left
				       (floor (- (gvl :text-width)
						 (gvl :text :width)) 2)))
			   (:right (+ base-left (- (gvl :text-width)
						   (gvl :text :width))))))))
	 (:text-width
	  ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-width-p)
			  (- (gv (kr-path 0 :parent :parent) :fixed-width-size)
			     (gvl :button-width) (gvl :text-offset) 8)
			  (gvl :text :width))))
	 (:height ,(o-formula (if (gv (kr-path 0 :parent :parent) :fixed-height-p)
				  (gv (kr-path 0 :parent :parent)
				      :fixed-height-size)
				  (+ 4 (MAX (gvl :text :height)
					    (gvl :button-width))))))

	 ;; Conditional formulas are required to allow either a list of
	 ;; strings or a list of string/function pairs in the :items slot.
	 (:item-obj ,(o-formula (nth (gvl :rank)
				     (gv (kr-path 0 :parent :parent) :item-objs))))
	 (:string ,(o-formula
		    (progn
		      ;; Invoke invalidate-demon on aggrelist's :items slot
		      ;; so that :fix-update-slots method will be called
		      (gv (kr-path 0 :parent) :items)
		      (nth (gvl :rank)
			   (gv (kr-path 1 :parent :parent) :strings)))))
	 (:action ,(o-formula (nth (gvl :rank)
				   (gv (kr-path 0 :parent :parent ) :actions))))
	 (:font ,(o-formula (gv (kr-path 0 :parent :parent) :font)))
	 (:parts
	  (:button
	   (:text :modify ,#'opal::Panel-Get-Label)
	   (:sel-box :omit)))
	 (:interactors
	  ((:press :omit) (:key :omit))))))
      (:SEL-BOX ,MOTIF-SELECTION-BOX)))

   (:interactors
    `((:PRESS ,inter:button-interactor
        (:start-where ,(o-formula (list :custom 
				   (gv-local :self :operates-on :button-list)
				   #'Motif-Element-Of-Not-Illegal)))
	(:window ,(o-formula (gv-local :self :operates-on :window)))
	(:how-set :list-toggle)
	(:final-function
	 ,#'(lambda (interactor final-obj-over)
	      (let* ((action (g-value final-obj-over :action))
		     (gadget (g-value interactor :operates-on))
		     (item-obj (g-value final-obj-over :item-obj))
		     (value-obj (g-value gadget :button-list :selected)))

		;; Propagate change toward :value slot
		(s-value gadget :value-obj value-obj)

		;; Global function executed whenever selections change
		(kr-send gadget :selection-function gadget
			 (g-value gadget :value))

		; If the button is selected, then execute the local function
		(when (member item-obj (g-value gadget :value)
			      :test #'equal)
		  (when action
		    (funcall action gadget item-obj)))))))

      (:KEY ,inter:button-interactor
       (:active ,(o-formula (and (gv-local :self :window)
				 (gv-local :self :operates-on
					   :keyboard-selection-p))))
       (:window ,(o-formula (gv-local :self :operates-on :window)))
       (:continuous NIL)
       (:start-where T)
       (:start-event (list #\space
			   :uparrow :rightarrow :downarrow :leftarrow))
       (:final-function
	,#'(lambda (interactor obj)
	     (declare (ignore obj))
	     (let* ((char (inter:event-char inter:*Current-Event*))
		    (gadget (g-value interactor :operates-on))
		    (selection (g-value gadget :keyboard-selection-obj)))
	       (case char
		 (#\space
		  (let ((item-obj (g-value gadget :keyboard-selection))
			(action (g-value selection :action))
			(value-obj (g-value gadget :button-list :selected)))

		    ;; Propagate new selection toward :value slot
		    (s-value gadget
			     :value-obj
			     (set-exclusive-or (list selection) value-obj))
		    
		    ;; Global function for all items
		    (kr-send gadget :selection-function
			     gadget (g-value gadget :value))
	       
		    ;; Local function assigned to item
		    (when (member item-obj (g-value gadget :value)
				  :test #'equal)
		      (when action
			(funcall action gadget item-obj)))))
		 #|
		 ;; If the object selected was active, then set the
		 ;; keyboard selection to be the object just selected
		 (:leftdown
		  (let ((obj (motif-element-of-not-illegal
			      (g-value gadget :button-list)
			      interactor inter:*Current-Event*)))
		    (when (and obj (g-value obj :active-p))
		      (s-value gadget
			       :keyboard-selection
			       (g-value obj :item-obj)))))
                 |#
		 ((:downarrow :rightarrow)
		  (let* ((prev-rank (g-value selection :rank))
			 (button-list (g-value gadget :button-list))
			 (buttons (g-value button-list :components))
			 (max-rank (g-value button-list :tail :rank)))
		    (do* ((rank (if (= prev-rank max-rank)
				    0 (+ 1 prev-rank))
				(if (= rank max-rank)
				    0 (+ 1 rank)))
			  (button (nth rank buttons) (nth rank buttons)))
			 ((g-value button :active-p)
			  (s-value gadget
				   :keyboard-selection
				   (g-value button :item-obj))))))

		 ((:uparrow :leftarrow)
		  (let* ((prev-rank (g-value selection :rank))
			 (button-list (g-value gadget :button-list))
			 (buttons (g-value button-list :components))
			 (max-rank (g-value button-list :tail :rank)))
		    (do* ((rank (if (> prev-rank 0)
				    (- prev-rank 1) max-rank)
				(if (> rank 0)
				    (- rank 1) max-rank))
			  (button (nth rank buttons) (nth rank buttons)))
			 ((g-value button :active-p)
			  (s-value gadget
				   :keyboard-selection
				   (g-value button :item-obj))))))))))))))


(s-value MOTIF-CHECK-BUTTON-PANEL :add-local-item
	 #'opal::Motif-Buttons-Add-Local-Item)
(s-value MOTIF-CHECK-BUTTON-PANEL :add-item
	 #'opal::Motif-Buttons-Add-Item)
(s-value MOTIF-CHECK-BUTTON-PANEL :remove-local-item
	 #'opal::Motif-Buttons-Remove-Local-Item)
(s-value MOTIF-CHECK-BUTTON-PANEL :remove-item
	 #'opal::Motif-Buttons-Remove-Item)
(s-value MOTIF-CHECK-BUTTON-PANEL :change-item
	 (g-value opal:aggrelist :change-item))
(s-value MOTIF-CHECK-BUTTON-PANEL :remove-nth-item
	 (g-value opal:aggrelist :remove-nth-item))
(s-value MOTIF-CHECK-BUTTON-PANEL :string-set-func
	 #'opal::Motif-Button-String-Func)


;;;
;;;  DEMO FUNCTION
;;;

#+garnet-debug
(defun MOTIF-CHECK-BUTTONS-GO (&key dont-enter-main-event-loop
				    not-double-buffered-p)
  (create-instance 'MOTIF-CHECK-BUTTONS-WIN inter:interactor-window
     (:double-buffered-p (not not-double-buffered-p))
     (:title "Motif Check Buttons")
     (:left 650)(:top 243)(:width 200)(:height 200))
  (s-value MOTIF-CHECK-BUTTONS-WIN
	   :aggregate
	   (create-instance 'MOTIF-CHECK-BUTTONS-TOP-AGG opal:aggregate))
  (create-instance 'DEMO-MOTIF-CHECK-BUTTON MOTIF-CHECK-BUTTON
     (:left 30) (:top 30))
  (create-instance 'DEMO-MOTIF-CHECK-BUTTON2 MOTIF-CHECK-BUTTON
     (:left 130) (:top 80)
     (:string (create-instance NIL opal:roundtangle
		(:width 40) (:height 30)
		(:filling-style opal:yellow-fill))))
  (create-instance 'DEMO-MOTIF-CHECK-BUTTON-PANEL MOTIF-CHECK-BUTTON-PANEL
     (:left 30) (:top 70))		   
  (opal:add-components MOTIF-CHECK-BUTTONS-TOP-AGG
		       (create-instance NIL MOTIF-BACKGROUND)
		       DEMO-MOTIF-CHECK-BUTTON DEMO-MOTIF-CHECK-BUTTON2
		       DEMO-MOTIF-CHECK-BUTTON-PANEL)
  ;;; Now set up global keyboard interactor
  (s-value DEMO-MOTIF-CHECK-BUTTON-PANEL :keyboard-selection-p T)
  (create-instance 'DEMO-MOTIF-CHECK-BUTTON-INTER MOTIF-TAB-INTER
     (:window MOTIF-CHECK-BUTTONS-WIN)
     (:objects (list DEMO-MOTIF-CHECK-BUTTON-PANEL DEMO-MOTIF-CHECK-BUTTON
		     DEMO-MOTIF-CHECK-BUTTON2)))
  (opal:update MOTIF-CHECK-BUTTONS-WIN)
  (unless dont-enter-main-event-loop #-cmu (inter:main-event-loop))
  )

#+garnet-debug 
(defun MOTIF-CHECK-BUTTONS-STOP ()
  (opal:destroy MOTIF-CHECK-BUTTONS-WIN))
