;;;
;;; config.lisp - Garnet configuration
;;;

(defpackage :garnet-user
  (:documentation "Garnet configuration and user package.")
  (:use :cl)
  (:export
   #:launch-process-p
   #:Garnet-Version-Number
   #:Garnet-Src-Pathname
   #:Garnet-Binary-Pathname
   #:Garnet-Lib-Pathname
   #:garent-pathnames
   #:Garnet-Bitmap-Pathname
   #:Garnet-Pixmap-Pathname
   #:Garnet-Gilt-Bitmap-Pathname
   #:Garnet-C32-Bitmap-Pathname
   #:Garnet-DataFile-Pathname
   #:Garnet-Gesture-Data-Pathname
   ))
(in-package :garnet-user)

(defvar launch-process-p NIL
  "Tell garnet-loader to not launch the main event loop process.")

(defparameter Garnet-Version-Number "3.0")
(pushnew :GARNET *features*)
(pushnew :GARNET-V3.0 *features*)

;;; The :GARNET-DEBUG option allows many different kinds of run-time checking,
;;; and also loads some extra test code.  After you have debugged your code
;;; and want it to run faster, remove :GARNET-DEBUG from the *features* list
;;; and RECOMPILE all of Garnet and your code.  The result will be smaller and
;;; somewhat faster.
;;; To remove :GARNET-DEBUG from the *features* list, either defvar
;;; Garnet-Garnet-Debug to NIL before you load the garnet-loader, or simply
;;; comment out the next few lines.
(defvar Garnet-Garnet-Debug T)
(if Garnet-Garnet-Debug
    (pushnew :garnet-debug *features*)
    (setf *features* (delete :garnet-debug *features*)))

;;; The :GARNET-PROCESSES keyword goes on the *features* list if this version
;;; of lisp supports multiple processes.  Then things like the animation
;;; interactor can use the #+garnet-processes switch, instead of referring
;;; explicitly to different versions of lisp.
#+(or allegro lispworks (and cmu mp))
(pushnew :GARNET-PROCESSES *features*)

;;; The :GARNET-BINS option controls whether Garnet uses its own constructed
;;; hash tables called "bins" or uses the system's hash tables at the kernel
;;; of the KR system.  Push :GARNET-BINS onto the *features* list for lisp
;;; implementations that compile to machine code and have slow hash tables.
;;; Don't push it for implementations like CLISP which have fast hash tables.
#-CLISP
(pushnew :GARNET-BINS *features*)

;;; The garnet-protected-eval feature is used to conditionalize the
;;; usage of the protected-eval module for Garnet error handling.  It
;;; also allows the process code to be set up right.
#+(or allegro (and cmu mp))
(pushnew :GARNET-PROTECTED-EVAL *features*)

(defvar Garnet-Readtable *readtable*
  "This variable is used by Allegro to restore the old value of the *readtable*
when a saved image is restarted (see opal:make-image in opal/utils.lisp).")

;; Set compiler optimization settings
;;
(defvar *default-garnet-proclaim*
  #+(or allegro lispworks apple) '(optimize (speed 3) (safety 1) (space 0)
                                   (debug #+garnet-debug 3 #-garnet-debug 0))
  #+cmu '(optimize (speed 3) (safety 1) (space 0))
  #-(or allegro cmu lispworks apple) NIL)

(when *default-garnet-proclaim*
  (proclaim *default-garnet-proclaim*))

;; @@@ How about move these package definitions to where their sources are?
(progn
  #|
  (defpackage :GEM (:use :COMMON-LISP :KR :KR-DEBUG))
  (defpackage :OPAL (:use :COMMON-LISP :KR))
  (defpackage :INTERACTORS (:use :COMMON-LISP :KR) (:nicknames :INTER)
    (:export *GARNET-BREAK-KEY* *LEFT-BUTTON* *TRANS-FROM-FILE*))
  (defpackage :GARNET-GADGETS (:use :COMMON-LISP :KR) (:nicknames :GG))
  (defpackage :GARNET-DEBUG (:use :COMMON-LISP :KR :OPAL) (:nicknames :GD))
  |#

  #|
  (defpackage :GILT (:use :COMMON-LISP :KR))
  (defpackage :C32 (:use :COMMON-LISP :KR))
  (defpackage :LAPIDARY (:use :COMMON-LISP :KR))
  (defpackage :AGATE (:use :COMMON-LISP :KR))
  |#

  #|
  (defpackage :DEMO-3D (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MULTIWIN (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MULTIFONT (:use :COMMON-LISP KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-ANIMATOR (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-ANGLE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-OTHELLO (:use :KR :COMMON-LISP) (:nicknames :DOTH)
    (:export DO-GO DO-STOP START-GAME STOP-GAME SET-SCORE))
  (defpackage :DEMO-PIXMAP (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-ARITH (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-SCHEMA-BROWSER (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP SCHEMA-BROWSER SCHEMA-BROWSER-WIN
	     SCHEMA-BROWSER-TOP-AGG))
  (defpackage :DEMO-ARRAY (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-SCROLLBAR (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP
	     MAC-obj MAC-Go MAC-Stop
	     Open-obj Open-Go Open-Stop
	     NEXT-obj NEXT-Go NEXT-Stop
	     Motif-obj Motif-Go Motif-Stop))
  (defpackage :DEMO-CLOCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-SEQUENCE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-EDITOR (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-TEXT (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-FILE-BROWSER (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP FILE-BROWSER FILE-BROWSER-WIN
	     FILE-BROWSER-TOP-AGG))
  (defpackage :DEMO-TRUCK (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GADGETS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-TWOP (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GESTURE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-UNISTROKES (:use :COMMON-LISP :KR :INTER) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GRAPH (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP SCHEMA-GRAPH DEMO-GRAPH-ERROR-GADGET ROOT-BOX
	     RELAYOUT DEMO-GRAPH-WIN))
  (defpackage :DEMO-VIRTUAL-AGG (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-GROW (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  (defpackage :DEMO-XASPERATE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-LOGO (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP RE-ANIMATE))
  (defpackage :DEMOS-CONTROLLER (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MANYOBJS (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MENU (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :GARNET-CALCULATOR (:use :COMMON-LISP :KR)
    (:export START-CALC STOP-CALC DO-GO DO-STOP))
  (defpackage :DEMO-MODE (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :GARNETDRAW (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :DEMO-MOTIF (:use :COMMON-LISP :KR) (:export DO-GO DO-STOP))
  (defpackage :MGE (:use :COMMON-LISP :KR)
    (:export DO-GO DO-STOP
	     CREATE-PIECE DESTROY-PIECE DESTROY-ALL-PIECES
	     GO-INITIALIZE EDITOR-SHOW-WINDOW))
  (defpackage :DEMO-MOVELINE (:use :KR :COMMON-LISP) (:export DO-GO DO-STOP))
  |#
  )

;;; launch-process-p controls whether Garnet will launch
;;; a separate process to detect keyboard and mouse events.
;; XXX @@@ Different than the previous defvar ???
(defvar launch-process-p T)

;;; update-locking-p controls whether process locks will be activated
;;; around the update method (this keeps two processes from calling update
;;; at the same time).
(defvar update-locking-p T
  "If T, uses process locks to keep Update in a process from interrupting
   itself in a different process.")

(defun Version-Error ()
  (error "Could not determine which compiled binaries are appropriate to
load into your lisp.  Please set common-lisp-user::Garnet-Version before loading
Garnet-Loader again."))

(defun Get-Garnet-Version ()
  ;; #+sparc    (or #+allegro-v4.0 :sparc-allegro
  ;;                #+allegro-v4.1 :sparc-allegro4.1
  ;;                #+allegro-v4.2 :sparc-allegro4.2
  ;;                #+cmu          :sparc-cmucl
  ;;                #-(and allegro-v4.0 allegro-v4.1 allegro-v4.2 cmu)
  ;; 		   (version-error))
  ;; #+dec3100  (or #+allegro-v3.1 :pmax-allegro
  ;;                #+allegro-v4.1 :pmax-allegro4.1
  ;;                #-(and allegro-v3.1 allegro-v4.1) (version-error))
  ;; #+(or pa hpux) #+allegro-v4.2 :hp-allegro4.2
  ;;                #-allegro-v4.2 (version-error)
  ;;#+lispworks :alpha-lw
  ;; #+clisp     :clisp
  ;; #+apple     :mac
  ;;#-(or sparc dec3100 pa hpux clisp lispworks apple) (version-error)

  ;; Just return a keyword for the implementation type
  (intern (string-upcase (lisp-implementation-type)) :keyword)
  )

;;; Garnet-Version should be set to :external for non-CMU users, and
;;; Your-Garnet-Pathname should be set appropriately.
;;;
(defvar garnet-version :external)  ; was (Get-Garnet-Version)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pathnames

(defvar Your-Garnet-Pathname
  (namestring (asdf:system-relative-pathname :garnet "")))

(defvar Garnet-Src-Pathname    Your-Garnet-Pathname) 
(defvar Garnet-Binary-Pathname Your-Garnet-Pathname)
(defvar Garnet-Lib-Pathname    Your-Garnet-Pathname)

(defun Garnet-Pathnames (subdir dir)
  (merge-pathnames subdir dir))

(defvar Garnet-Bitmap-Pathname
  (garnet-pathnames #-apple "bitmaps/" #+apple "bitmaps:" Garnet-Lib-Pathname))
(defvar Garnet-Pixmap-Pathname
  (garnet-pathnames #-apple "pixmaps/" #+apple "pixmaps:" Garnet-Lib-Pathname))
(defvar Garnet-Gilt-Bitmap-Pathname
  (garnet-pathnames #-apple "gilt/" #+apple "gilt:" Garnet-Lib-Pathname))
(defvar Garnet-C32-Bitmap-Pathname
  (garnet-pathnames #-apple "c32/" #+apple "c32:" Garnet-Lib-Pathname))
(defvar Garnet-DataFile-Pathname
  (garnet-pathnames #-apple "data/" #+apple "data:" Garnet-Lib-Pathname))
(defvar Garnet-Gesture-Data-Pathname
  (garnet-pathnames #-apple "gesture/" #+apple "gesture:" Garnet-Lib-Pathname))

;; End
