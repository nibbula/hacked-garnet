;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 22-May-94 Andrew Mickish -- Used (gv :self) instead of kr::*schema-self*
;;; 08-Apr-94 Andrew Mickish -- Changed cursor height for Mac version
;;; 20-Dec-93 Andrew Mickish -- Fixed gem:Text-Extents call to take a font
;;;               as a parameter, not the text object.
;;; 06-Oct-93 Andrew Mickish -- Removed obsolete :text-extents slot; used
;;;               WIDTH returned by xlib:text-extents instead of difference
;;;               between LEFT-BEARING and RIGHT-BEARING
;;; 17-Sep-93 Andrew Mickish -- Removed left-bearing from :draw method;
;;;               fixed :width formula accordingly
;;;  9-Sep-93 Andrew Mickish -- Fixed :cursor-offset formula to put the cursor
;;;               more between characters
;;;  1-Sep-93 Andrew Mickish -- Used :cut-string-structs slot instead of
;;;               kr::self-old-value; Created :prev-len to replace :x-substr
;;; 30-Aug-93 Andrew Mickish -- Fixed Move-Cursor-Up/Down-One-Line
;;; 17-Aug-93 Andrew Mickish -- Removed reference to kr::*NO-VALUE*
;;; 30-Jul-93 Andrew Mickish -- New opal:text obviates all cursor- and multi-
;;;               text versions
;;; 10-Jun-93 Andrew Mickish -- Renamed black-xor-hack to HP-XOR-Hack
;;;  6-Apr-93 koz Converted with-*-styles macros to set-*-style fns
;;;               And omitted "clip-mask" as argument to draw function.
;;;               And changed hack to "black-xor-hack" (now a macro)
;;; 15-Apr-92 ecp Fixed bug where cursor was not appearing on color screen
;;;                 with black = 0 when draw-function = :xor.
;;; 23-Oct-91 ecp Fix for when drawing cursor on color screen with black = 0.
;;;  4-Feb-91 ecp Cursor of cursor-multi-text now has draw function :xor.
;;; 23-Mar-90 ecp  New slot :fill-background-p for text objects.
;;; 14-Mar-90 ecp Move-cursor-* functions added.
;;; 28-Feb-90 ecp Cursor of cursor-multi-text now has same draw
;;;		  function as the text itself.
;;;
(in-package "OPAL")

(defvar *cursor-width*          2)
(defvar *cursor-half-width*     (floor *cursor-width* 2))
(defvar *cursor+++half+++width* (ceiling *cursor-width* 2))
(defvar *cursor-draw-fn*        (get :xor :x-draw-function))

(create-instance 'opal:TEXT opal:graphical-object
  :declare ((:parameters :left :top :string :font :actual-heightp
			 :justification :fill-background-p :line-style
			 :cursor-index :draw-function :visible)
	    (:type (string :string)
                   ((or (is-a-p opal:font) (is-a-p opal:font-from-file)) :font)
		   ((member :left :center :right) :justification))
	    (:maybe-constant :left :top :string :font :actual-heightp
                             :line-style :visible)
	    (:local-only-slots (:cursor-index nil) (:window nil)
                               (:parent nil) (:cut-string-structs NIL))
            (:ignored-slots :depended-slots :update-slots :update-slots-values
                            :xfont :cut-string-structs
			    :cut-strings)
	    (:update-slots :visible :fast-redraw-p :top :left :width :height
			   :string :xfont :actual-heightp :fill-background-p
			   :line-style :draw-function :cursor-offset
			   :justification :cut-strings :line-number))
  (:string            "")
  (:font              opal:default-font)
  (:actual-heightp    nil)
  (:fill-background-p nil)
  (:cursor-index NIL)
  (:justification :left)
  (:xfont (o-formula (gvl :font :xfont)))
  (:cut-strings
   (o-formula
    (let* ((string (gvl :string))
	   (font (gvl :xfont))
	   ;; Structs will be NIL if formula has never been evaluated
	   (structs (g-value (gv :self) :cut-string-structs)))
      (do* ((old-structs structs (cdr old-structs))
	    (struct (car old-structs) (car old-structs))
	    (i -1 j)
	    (j 0)
	    (substring nil))
	   ((null i) (progn
		       ;; Throw away old cut-strings that we didn't use
		       (when old-structs
			 (let ((last-cdr (nthcdr (- (length structs)
						    (length old-structs)
						    1)
						 structs)))
			   (setf (cdr last-cdr) NIL)))
		       structs))
	(setf j (position #\Newline string :start (1+ i))
	      substring (if (or j substring)
			    (subseq string (1+ i) j)
			    string))
	(multiple-value-bind (width dummy2 dummy3 left-bearing)
	    (gem:text-extents (or (gvl :window)
				  (gv device-info :current-root))
			      (gvl :font) substring)
	  (declare (ignore dummy2 dummy3))
	  ;; Reuse an old struct, if possible
	  (cond
	    (struct
	     (setf (cut-string-string struct) substring)
	     (setf (cut-string-width struct) width)
	     (setf (cut-string-left-bearing struct) left-bearing))
	    (t (setf structs
		     ;; Note: only append when we're adding a new line, and
		     ;; the object has never had this many lines.
		     (append structs
			     (list (make-cut-string
				    :string substring
				    :width width
				    :left-bearing left-bearing))))))))
      (s-value (gv :self) :cut-string-structs structs))))
  (:height (o-formula (* (gvl :font :font-height)
			 (length (gvl :cut-strings)))))
  (:width (o-formula (let ((width *cursor-width*))
		       (dolist (cstring (gvl :cut-strings))
			 (setq width (max width (cut-string-width cstring))))
		       width)))
  (:line-number (o-formula (cursor-index-to-line-number
			    (gvl :cut-strings) (gvl :cursor-index))))
  (:line-height (o-formula (let ((root (gvl :window))
				 (font (gvl :font)))
			     (+ (gem:max-character-ascent root font)
				(gem:max-character-descent root font)))))
  (:prev-len
   (o-formula
    (let ((cursor-index (gvl :cursor-index)))
      (when cursor-index
	(let* ((cut-strings (gvl :cut-strings))
	       (line-number (gvl :line-number))
	       (n 0)   (prev-len 0))
	  ;; Count up all the characters in the lines before the cursor's line
	  (dolist (a-cut-string cut-strings)
	    (if (eq n line-number)
		(return prev-len)
		(progn
		  (setf prev-len
			;; Add an extra 1 for the #\newline
			(+ 1 prev-len
			   (length (cut-string-string a-cut-string))))
		  (incf n)))))))))
  (:cursor-offset
   (o-formula
    (let ((cursor-index (gvl :cursor-index)))
      (when cursor-index
      (let* ((cut-string        (nth (gvl :line-number) (gvl :cut-strings)))
	     (justification     (gvl :justification))
	     (string            (cut-string-string cut-string))
	     (line-width        (cut-string-width cut-string))
	     (max-line-width    (gvl :width))
	     (font              (gvl :font))
	     (char-width        (gv  font :char-width))
	     (prev-len          (gvl :prev-len))
	     ;; Adj-index gives us the cursor-index on the particular line
	     (adj-index   (- cursor-index prev-len))
	     (fixed-index (cond ((<= cursor-index 0)                 0)
				((>= adj-index line-width)  line-width)
				(T                           adj-index)))
	     (base-width  (if char-width
			      (* char-width fixed-index)
			      (gem:text-width (gvl :window) font
					      (subseq string 0 fixed-index))))
	     (max-cursor-offset (- max-line-width *cursor+++half+++width*)))
	;; Never let offset be less than zero
	(max *cursor-half-width*
	(min max-cursor-offset
	     (+ (max (min max-cursor-offset base-width)
		     *cursor-half-width*)
		(case justification
		  (:right (- max-line-width (max line-width 2)))
		  (:center (floor (- max-line-width line-width) 2))
		  (t 0)))))))))))


(create-instance 'opal::CURSOR-TEXT opal::text)
(create-instance 'opal::MULTI-TEXT opal::text)
(create-instance 'opal::CURSOR-MULTI-TEXT opal::multi-text)


(define-method :draw opal:text (gob a-window)
  (let* ((update-vals   (g-local-value gob :update-slots-values))
	 (font (g-value gob :font))
         (lstyle (aref update-vals opal::+text-lstyle+)))
    (if (and lstyle font)
      (let* ((left           (aref update-vals opal::+text-left+))
	     (top            (aref update-vals opal::+text-top+))
	     (cursor-offset  (aref update-vals opal::+text-cursor-offset+))
	     (cut-strings    (aref update-vals +text-cut-strings+))
	     (max-line-width (aref update-vals +text-width+))
	     (justification  (aref update-vals +text-justification+))
	     (line-number    (aref update-vals +text-line-number+))
	     (ascent 	     (gem:max-character-ascent a-window font))
	     (line-height    (+ ascent (gem:max-character-descent
                                        a-window font))))
	(do ((count 0 (1+ count))
	     (remaining cut-strings (cdr remaining)))
	    ((null remaining))
	  (let* ((cut-string (car remaining))
		 (width (cut-string-width cut-string))
		 (string (cut-string-string cut-string)))
	    (gem:draw-text
	     a-window
	     (+ left (case justification
		       (:right (- max-line-width width))
		       (:center (floor (- max-line-width width) 2))
		       (t 0)))
	     (+ top ascent (* count line-height))
	     string font (aref update-vals opal::+text-draw-function+)
	     lstyle (aref update-vals +text-fill-background-p+))))
	(when cursor-offset
	  (let ((cursor-left (+ left cursor-offset))
		(cursor-top (+ top (* line-number line-height))))
	    (gem:draw-line a-window
                           cursor-left
                           #-apple cursor-top #+apple (+ 2 cursor-top)
		           cursor-left
                           #-apple (+ cursor-top line-height)
                           #+apple (- (+ cursor-top line-height) 4)
		           :XOR
		           opal:line-2)))))))


(defun cursor-index-to-line-number (cut-strings index)
  (when index
    (let (length-of-this-line)
      (dotimes (line-num (length cut-strings))
	(setq length-of-this-line
	      (length (cut-string-string (car cut-strings))))
	(if (<= index length-of-this-line)
	    (return line-num)
	    (progn
	      (setq index (- index 1 length-of-this-line))
	      (setq cut-strings (cdr cut-strings))))))))

(defun move-cursor-down-one-line (gob)
  (when (g-value gob :cursor-index)
    (let* ((cut-strings (g-value gob :cut-strings))
	   (line-height (g-value gob :line-height))
	   (line-number (g-value gob :line-number)))
      (when (< line-number (1- (length cut-strings)))
	(s-value gob :cursor-index
		 (opal::get-cursor-index
		  gob
		  (+ (g-value gob :left) (g-value gob :cursor-offset))
		  (+ (g-value gob :top)
		     (* line-height (1+ line-number)))))))))

(defun move-cursor-up-one-line (gob)
  (when (g-value gob :cursor-index)
    (let* ((line-height (g-value gob :line-height))
	   (line-number (g-value gob :line-number)))
      (when (> line-number 0)
	(s-value gob :cursor-index
		 (opal::get-cursor-index
		  gob
		  (+ (g-value gob :left) (g-value gob :cursor-offset))
		  (+ (g-value gob :top)
		     (* line-height (1- line-number)))))))))

(defun move-cursor-to-beginning-of-line (gob)
  (let ((index (g-value gob :cursor-index)))
    (if index
        (s-value gob :cursor-index (g-value gob :prev-len))
        (s-value gob :cursor-index 0))))

(defun move-cursor-to-end-of-line (gob)
  (if (g-value gob :cursor-index)
      (let* ((cut-strings (g-value gob :cut-strings))
	     (line-number (g-value gob :line-number)))
	(s-value gob :cursor-index
		 (+ (g-value gob :prev-len)
		    (length (cut-string-string
			     (nth line-number cut-strings))))))
      (s-value gob :cursor-index (length (g-value gob :string)))))


(define-method :string-set-func OPAL::TEXT
    (gadget-obj str-obj final-event final-string)
  (declare (ignore final-event))
  (if (eq str-obj gadget-obj)
      ; then is me (otherwise, is probably an error)
      (s-value str-obj :string final-string)
      ; else return NIL
      NIL))
