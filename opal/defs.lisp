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
;;; Opal:Defs.Lisp
;;;
;;; This file contains all the defvars, defconstants, defstructs, etc.,
;;;  which are used by Opal.  This does not contain any defmacros, however.
;;;  This file also contains the export list for Opal.
;;;
;;; Change Log:
;;;     date     who    what
;;;     ----     ---    ----
;;;    8-Dec-95  goldman Added variable *read-write-colormap-cells-p*
;;;                        See documentation string thereof.
;;;   12-Sep-95  goldman   Changed the way colormap entries are tracked in
;;;                        from using a fixed-size array to using a hash-table.
;;;                        Applied a patch from Nick Levine that makes it
;;;                        possible for garnet to deal with True Color displays.
;;;   25-May-94  amickish Made setf of *function-alist* a GEM method
;;;   17-Dec-93  amickish :x-image ---> :device-image in HALFTONE defstruct
;;;    5-Dec-93  amickish Removed Do-Defs-Initializations (temporary GEM fn)
;;;   30-Sep-93  amickish Supported gray-scale screens in initialize-x11-values
;;;   30-Aug-93  amickish Moved defvar of *cursor-width* to text.lisp
;;;   20-Aug-93  rajan    Exported motif-light-xxx and motif-light-xxx-fill
;;;    6-Aug-93  amickish Added opal:Arrow-Pair
;;;   16-Jul-93  amickish Moved Set-Draw-Functions here from basics.lisp
;;;   10-Jun-93  Jim Davis  Bound *print-pretty* to NIL while calling princ-to-
;;;                         string when trying to determine if screen is color.
;;;   10-Jun-93  amickish Set *HP-display-type?* for HP-XOR-Hack
;;;   20-May-93  amickish restored conditional definition of *function-alist*
;;;    6-Apr-93  koz    removed defunct with-*-styles
;;;   18-Jan-93  amickish Added char-width, change-cursors, and restore-cursors
;;;                       to export list
;;;    5-Jan-93  amickish Removed *is-this-a-color-screen-and-is-black-zero*
;;;   15-Dec-92  amickish Unexported opal:type-check and opal:window
;;;   10-Dec-92  amickish *drawable-to-window-mapping* ---> *garnet-windows*
;;;   25-Nov-92  amickish Exported gray-line
;;;   22-Oct-92  koz    added zoom-window and fullzoom-window to exports
;;;   11-Jun-92  ecp    Altered *twopi* due to bug in CMUCL 16.
;;;    9-Jun-92  amickish Exported white-line
;;;   29-May-92  ecp/ky Determine display number and screen number from
;;;			full display name.
;;;   21-Apr-92  ecp    Added main-event-loop-process-running-p
;;;   20-Apr-92  Poelman added string-upcase calls when checking color screen
;;;    2-Apr-92  rgm    added set-standard-font; moved export of multifont
;;;			routines to multifont.lisp
;;;    1-Apr-92  ecp    Must undo change of 26-Feb-92 in CMUCL.
;;;   31-Mar-92  ecp    It is necessary to have a third case when declaring
;;;			*function-alist*, for color screens where white=1,
;;;			such as the HP machine.
;;;   31-Mar-92  bam    Renamed initialize-virtual-aggregate-bboxes to be
;;;			       recalculate-virtual-aggregate-bboxes
;;;   20-Mar-92  ecp    Moved exports here from virtual-aggregates and multifont.
;;;   10-Mar-92  ecp    Gave halftone new filling-style field.
;;;   27-Feb-92  ecp    Added deiconify-window.
;;;   26-Feb-92  ecp    Must call xlib:open-display a second time when getting
;;;			*default-x-colormap*.
;;;    6-Feb-92  ecp    Added leaf-objects-in-rectangle, components-in-rectangle,
;;;                     and obj-in-rectangle.
;;;   31-Jan-92  ecp    Eliminated *display-name-to-display-mapping*
;;;   26-Nov-91  ecp    Use *copy* instead of *clear* for erasing buffers.
;;;   26-Mar-91  ecp    kcl patch
;;;    7-Mar-91  ecp    The question of whether the screen is color or
;;;                     black-and-white is now determined inside
;;;			initialize-default-x-values.
;;;   22-Feb-91  amickish  New exported motif colors and filling styles.
;;;   21-Feb-91  ecp    New exported variables *screen-width* and
;;;			*screen-height*, which are the width and height
;;;			of the screen.  Also iconify-window.
;;;   25-Oct-90  ecp    New exported commands opal:raise-window and
;;;		        opal:lower-window which move window to front or
;;;			back of screen.
;;;   11-Sep-90  ecp    Get display name in allegro by (sys::getenv "DISPLAY").
;;;                     Use (short-site-name) as an #+allegro alternative to
;;;                     (machine-instance).
;;;   15-Aug-90  ecp    Exporting destroy-me.
;;;                     Moved lots of initialization stuff
;;;			into new function initialize-default-x-values.
;;;    8-Aug-90  ecp    Use #+(and allegro clx-mit-r4) "" in
;;;			*default-x-display-name*
;;;   26-Jun-90  ecp    Due to temporary bug in clx, had to
;;;			coerce *twopi* to an short-float.
;;;   21-Jun-90  nesmith
;;;			Use #+allegro (short-site-name) in
;;;			*default-x-display-name*
;;;   19-Jun-90  ecp    New functions gv-center-x-is-center-of,
;;;			gv-center-y-is-center-of,
;;;			gv-right-is-left-of, gv-bottom-is-top-of.
;;;   18-Jun-90  ecp    Added *clear* for erasing buffers.
;;;    5-Jun-90  chris  Added lispworks.
;;;   14-Mar-90  ecp    Move-cursor-* functions added.
;;;    9-Mar-90  ecp    Changed *function-alist* again to try
;;;			to deal with "xor problem".
;;;			Moved lots of defvars here from new-defs.
;;;			New variables *white* and *black*.
;;;   13-Feb-90  ecp    Implemented color.
;;;   26-Jan-90  bam    Added :key-press and :button-press to
;;;                     *exposure-event-mask*
;;;   13-Dec-89  ecp    Changed #+lucid to #-cmu in declaration of
;;;                     *function-alist*
;;;   14-Jun-89  koz    Created.  Simply extracted all the def* from all the
;;;			Opal files.  No modifications were made to them.

;;;---------------------------------------------------------------------------
;;; Local changes
;;; [1995/09/11:goldman] incorporated Nick Levine's patch to determine
;;; whether we have a color screen.
;;;---------------------------------------------------------------------------

(in-package "OPAL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Export List  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This is the export list for *some* of OPAL
#|
(eval-when (eval load compile)
  (export '(bottom right center-x center-y
	    gv-bottom gv-right gv-center-x gv-center-y
	    gv-center-x-is-center-of gv-center-y-is-center-of
	    gv-right-is-left-of gv-bottom-is-top-of
	    top-side left-side bottom-side right-side
	    center set-center
	    bounding-box set-bounding-box
	    set-position set-size
	    draw erase rotate
	    initialize calculate-bounding-box point-in-gob
	    halftone halftone-darker halftone-lighter
	    halftone-image halftone-image-darker halftone-image-lighter
	    read-image write-image
	    add-component remove-component move-component
	    add-components remove-components remove-all-components
	    do-components do-all-components
	    point-to-component point-to-leaf
	    set-aggregate-hit-threshold
	    update destroy destroy-me
	    raise-window lower-window iconify-window deiconify-window
	    zoom-window fullzoom-window
	    
	    ;; Class names
	    aggregate view-object graphical-object line rectangle
	    roundtangle multipoint polyline polygon text bitmap arc oval
	    circle arrowhead multi-text cursor-multi-text
	    
	    line-style default-line-style filling-style default-filling-style
	    font cursor-text graphic-quality font-from-file cursor-font
	    arrow-cursor arrow-cursor-mask arrow-pair
	    hourglass-cursor hourglass-cursor-mask hourglass-pair
	    garbage-cursor garbage-cursor-mask garbage-pair
	    with-hourglass-cursor with-cursor default-font
	    display-info-display display-info-screen
	    display-info-root-window display-info-line-style-gc
	    display-info-filling-style-gc
	    convert-coordinates get-cursor-index string-width string-height
	    change-cursors restore-cursors char-width
	    move-cursor-down-one-line
	    move-cursor-up-one-line
	    move-cursor-to-beginning-of-line
	    move-cursor-to-end-of-line
	    
	    Get-X-Cut-Buffer Set-X-Cut-Buffer	;; for interactors' use
	    leaf-objects-in-rectangle components-in-rectangle obj-in-rectangle
	    
	    ;; filling and line style constants
	    no-fill black-fill white-fill
	    gray-fill light-gray-fill dark-gray-fill
	    red-fill green-fill blue-fill yellow-fill
	    cyan-fill orange-fill purple-fill
	    motif-gray-fill motif-blue-fill motif-orange-fill motif-green-fill
	    motif-light-gray-fill motif-light-blue-fill motif-light-orange-fill
	    motif-light-green-fill
	    
	    make-filling-style
	    diamond-fill
	    
	    no-line thin-line line-0 line-1 line-2 line-4 line-8 gray-line
	    dotted-line dashed-line 
	    red-line green-line blue-line yellow-line
	    cyan-line orange-line purple-line white-line
	    
	    ;; size of screen
	    *screen-width* *screen-height*
	    
	    ;; Colors
	    color white black red green blue cyan yellow orange purple
	    motif-gray motif-blue motif-orange motif-green motif-light-gray
	    motif-light-blue motif-light-orange motif-light-green
	    
	    ;; From Clean-Up.Lisp
	    clean-up change-garnet-display update-all reset-cursor
	    
	    ;; From open-and-close.lisp
	    disconnect-garnet reconnect-garnet
	    
	    ;; From process.lisp
	    launch-main-event-loop-process
	    kill-main-event-loop-process
	    main-event-loop-process-running-p
	    running-main-event-loop-process-elsewhere-p
	    
	    ;; From virtual-aggregates.lisp
	    virtual-aggregate remove-item add-item change-item point-to-rank
	    recalculate-virtual-aggregate-bboxes do-in-clip-rect
	    
	    get-standard-font
	    )))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; DefConstants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +twopi+ (min (* 2 pi) (coerce (* 2 pi) 'short-float)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; DefParameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Added :button-press and :key-press so garnet-debug:ident will work.
(defparameter *exposure-event-mask* nil)


;;; These two 2x2x2 arrays are used as a correction to a flaw in xlib:draw-arc
(defparameter *left-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (0 1)) ((0 1) (0 1)))))
(defparameter *top-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (0 0)) ((0 0) (0 1)))))
(defparameter *width-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (0 1)) ((0 1) (0 1)))))
(defparameter *height-adjustment*
  (make-array '(2 2 2) :initial-contents '(((0 1) (1 1)) ((1 1) (0 1)))))


;;; Routines used to get name of display, and extract
;;; display number and screen number.
;;; Normally, the name of a display is of the form
;;; "displayname:displaynumber.screennumber"
;;;
(defun get-full-display-name ()
   #+cmu
   (cdr (assoc :DISPLAY lisp::*environment-list*))
   #+(or allegro lispworks kcl clisp)
   (sys::getenv "DISPLAY")
   #+(and lucid lcl3.0)
   (lucid-common-lisp:environment-variable "DISPLAY")
   #+(and lucid (not lcl3.0))
   (system:environment-variable "DISPLAY")
   )

(defun get-display-name (display)
  (do* ((dlist (coerce display 'list) (cdr dlist))
        (c (car dlist) (car dlist))
        (namelist nil)
        )
       ((or (eq c nil) (eq c '#\:)) (coerce (reverse namelist) 'string))
       (push c namelist)))


(defun get-screen-number (display)
  (let* ((dlist (coerce display 'list))
         (numstr (progn
                   (do ((c (pop dlist) (pop dlist)))
                       ((or (eq c nil) (eq c '#\:))))
                   (do ((c (pop dlist) (pop dlist)))
                       ((or (eq c nil) (eq c '#\.))))
                   (do ((c (pop dlist) (pop dlist))
                        (numlist nil)
                        )
                       ((or (eq c nil) (eq c '#\.))
                        (coerce (reverse numlist) 'string))
                       (push c numlist)
                       )
                   ))
         (num (if (equal numstr "") 0 (read-from-string numstr)))
         )
    num))



;;; The :current-root slot of the following schema indicates the current
;;; device.  This is used for all calls to Gem which occur in places where
;;; explicit device information is not available.
;;; The :active-devices slot contains the list of all the devices that
;;; have been initialized.
;;;
(create-schema 'DEVICE-INFO
  (:current-root NIL)
  (:active-devices NIL))

;;; DZG - this is unused (defvar *default-x-full-display-name*)
(defvar *default-x-display-name*)
(defvar *default-x-display*)
(defvar *default-x-screen-number*)
(defvar *default-x-screen*)
(defvar *default-x-root*)
(defvar *default-x-colormap*)
(defvar *screen-width*)
(defvar *screen-height*)
(defvar *white*)
(defvar *black*)
(defvar *function-alist*)
(defvar *copy*)


;;replaced old *colormap-index-table* in array form with a hash-table
;;[1995/09/12:goldman]
;; Have to figure out what this really is.
;(defvar *colormap-index-table-size* 256)
;(defvar *colormap-index-table*
;  (make-array *colormap-index-table-size* :initial-element 0))
(defvar *colormap-index-table*
    ;;arguments to make-hash-table NOT well thought out! [1995/09/12:goldman]
  (make-hash-table :size 256))

(defvar *is-this-a-color-screen?* nil)
(defvar *read-write-colormap-cells-p* nil
  "This variable will be t if the screen type is :direct-color or :pseudo-color.")
(defvar *HP-display-type?* nil)



(defun set-draw-functions ()
  ;; Alist since CLX likes to get the draw function in the form of an
  ;; integer.  We want to specify nice keywords instead of those silly
  ;; numbers.
  (gem:set-draw-function-alist (gv DEVICE-INFO :current-root))
  (dolist (fn-pair *function-alist*)
    (setf (get (car fn-pair) :x-draw-function) (cdr fn-pair))))


;;; This is also called in reconnect-garnet.
;;;
(defun initialize-x11-values (full-display-name root-window)
  (setq *default-x-display-name*
	(if full-display-name
	  (get-display-name full-display-name)
	  #-(or allegro clisp) (machine-instance)
	  #+clisp ""
	  #+allegro (short-site-name)))
  (setq *default-x-screen-number* (get-screen-number full-display-name))

  ;; Set up all the Opal variables used to identify display, screen, etc.
  ;; Unfortunately, these are needed by discard-all-pending-events (in
  ;; process.lisp), which is called by launch-main-event-loop-process.
  (gem:set-device-variables root-window)

  ;; This is really dumb, but it's the only way I can think of
  ;; to find out if the screen is color or not.
;  (let* ((*print-pretty* NIL)
;         (colormap-string (string-upcase
;                           (princ-to-string opal::*default-x-colormap*))))
;    (if (or (search "PSEUDO-COLOR" colormap-string)
;            (search "DIRECT-COLOR" colormap-string)
;            (search "GRAY-SCALE" colormap-string))
;      (setq *is-this-a-color-screen?* t)
;      (setq *is-this-a-color-screen?* nil)))
  ;;incorporated Nick Levine's patch into the code.
  ;;[1995/09/11:goldman]
  ;;further patched this to add *read-write-colormap-cells-p* [1995/12/08:goldman]
  (let ((color-screen-types '(:pseudo-color :direct-color :static-color :true-color))
	(screen-type (xlib::visual-info-class
		      (xlib::screen-root-visual-info *default-x-screen*))))
    (setq *is-this-a-color-screen?*
      (if (member screen-type color-screen-types :test #'eq)
	  screen-type
	nil))
  (setq *read-write-colormap-cells-p* 
    (and *is-this-a-color-screen?*
	 (member screen-type '(:direct-color :pseudo-color) :test #'eq))))
    
  (with-constants-disabled
    (s-value opal::COLOR :color-p *is-this-a-color-screen?*))
  (setq *HP-display-type?* (and *is-this-a-color-screen?* (zerop *black*)))
  )
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;  DefVars  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *halftone-table-size* 17)
(defvar *halftone-table* nil) ;;; used to be set to (build-halftone-table)
                              ;;; but now that's a forward reference.  So,
                              ;;; now we setq this after defining that fn.

(defvar *default-text-extents* (make-list 9 :initial-element 0))

(defvar no-fill nil)
(defvar no-line nil)

(defvar *garnet-windows* NIL)

;; debugging tools
(defvar *event-debug* nil)
(defvar *expose-throw-aways* 0)

(defvar *opal-window-count* 0)

(defvar diamond-fill NIL)		;; set in halftones.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;  DefStructs  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This defstruct generates the functions Make-Halftone, Copy-Halftone,
;;; Halftone-Percent, Halftone-Device-Image, Halftone-Filling-Style, and
;;; Halftone-P.
(defstruct (HALFTONE (:print-function halftone-print))
  (percent 0)
  (device-image nil)
  (filling-style nil))

;;; This defstruct generates the functions Make-Display-Info, Copy-Display-Info,
;;; Display-Info-Display, Display-Info-Screen, Display-Info-Root-Window,
;;; Display-Info-Line-Style-GC, and Display-Info-Filling-Style-GC.
(defstruct (DISPLAY-INFO (:print-function display-info-printer))
  display
  screen
  root-window
  line-style-gc
  filling-style-gc)

;; modularity hack
(defmethod gem::%display-info-display ((info display-info))
  (display-info-display info))

;;; This defstruct generates the functions Make-Cut-String, Copy-Cut-String,
;;; Cut-String-String, Cut-String-Width, and Cut-String-Left-Bearing.
(defstruct CUT-STRING
  string
  width
  left-bearing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; DefSetfs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Accessors that do calculation from basic gob properties

;;; The accessors for the bottom and right of the gob, make it easier to
;;; adjust the far side of the gob's bounding box.

(defsetf bottom (gob) (value)
  `(setf (g-value ,gob :top) (1+ (- ,value (g-value ,gob :height)))))

(defsetf right (gob) (value)
  `(setf (g-value ,gob :left) (1+ (- ,value (g-value ,gob :width)))))

;;; The accessors for the sides of the gob adjust both the dimensions, and
;;; position of the gob based on the given value.

(defsetf left-side (gob) (value)
  `(progn
     (setf (g-value ,gob :width)
           (- (g-value ,gob :width) (- ,value (g-value ,gob :left))))
     (setf (g-value ,gob :left) ,value)))

(defsetf right-side (gob) (value)
  `(setf (g-value ,gob :width)
         (+ (g-value ,gob :width) (- ,value (right ,gob)))))

(defsetf top-side (gob) (value)
  `(progn
     (setf (g-value ,gob :height)
           (- (g-value ,gob :height) (- ,value (g-value ,gob :top))))
     (setf (g-value ,gob :top) ,value)))

(defsetf bottom-side (gob) (value)
  `(setf (g-value ,gob :height)
         (+ (g-value ,gob :height) (- ,value (bottom ,gob)))))

;;; The following allow access and setting to the gobs center
;;; position.

(defsetf center-x (gob) (value)
  `(setf (g-value ,gob :left)
         (- ,value (truncate (g-value ,gob :width) 2))))

(defsetf center-y (gob) (value)
  `(setf (g-value ,gob :top)
         (- ,value (truncate (g-value ,gob :height) 2))))
