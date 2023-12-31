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
;;; 20-Oct-93 Mickish    Added Demo-Unistrokes
;;; 24-Feb-93 Mickish    Removed references to compile-opal/inter-p
;;; 19-Feb-93 Mickish    Demo-Circle ---> Demo-Virtual-Agg
;;; 03-Feb-93 Mickish    Demo-Calculator ---> Garnet-Calculator
;;;  4-Jun-92 Myers	 Added demo-animator
;;; 30-Apr-92 Pervin     Removed demo-fade (it's now demo-logo).
;;; 28-Apr-92 Mickish    Added garnetdraw
;;; 13-Apr-92 Mickish    Added demo-logo.
;;; 02-Mar-92 Mickish    Added load of gestures, demo-gesture.
;;; 27-Mar-92 Pervin     Added demo-circle.
;;; 27-Mar-91 Pervin     Only load aggregraphs and gadgets if not compiling
;;;			 all of Garnet.
;;; 22-Mar-91 Pervin	 Added load of aggregraphs and gadgets.
;;; 15-Mar-91 Mickish    Added demo-graph, demo-truck
;;; 14-Mar-91 Pervin     Added demo-motif.
;;; 12-Oct-90 Osamu	 Added demo-xasperate, demo-calculator, demos-controller.
;;;  3-Aug-90 Myers	 Added demo-fade.
;;;  1-Aug-90 Pervin     Added demo-arith.
;;; 16-Jul-90 Pervin     Added demo-file-browser and demo-schema-browser.
;;; 12-Apr-90 Mitchell   Added #+allegro (gc t)
;;; 2-Apr-90 Cook/Pervin Added #+explorer part.
;;;

(in-package "USER")

(unless (get :garnet-modules :multifont)
  (load (user::garnet-pathnames "multifont-loader" user::Garnet-Opal-PathName)))
(unless (get :garnet-modules :aggregraphs)
  (load user::Garnet-Aggregraphs-Loader))
(unless (get :garnet-modules :gadgets)
  (load user::Garnet-Gadgets-Loader))
(unless (get :garnet-modules :ps)
  (load user::Garnet-PS-Loader))
(unless (get :garnet-modules :gesture)
  (load user::Garnet-Gesture-Loader))


  (defvar Garnet-Demos-Files   ;; defvar rather than defparameter so can setq
			     ;; this variable before loading if only want
			     ;; to compile some of these files
  '(
    "demo-3d"
    "demo-angle"
    "demo-animator"
    "demo-arith"
    "demo-array"
    "garnet-calculator"
    "demo-virtual-agg"
    "demo-clock"
    "demo-editor"
    "demo-file-browser"
    "demo-gadgets"
    "demo-gesture"
    "demo-graph"
    "demo-grow"
    "demo-logo"
    "demo-manyobjs"
    "demo-menu"
    "demo-mode"
    "demo-motif"
    "demo-moveline"
    "demo-multifont"
    "demo-multiwin"
    "demo-pixmap"
    "demo-schema-browser"
    "demo-scrollbar"
    "demo-sequence"
    "demo-text"
    "demo-truck"
    "demo-twop"
    "mge"
    "demo-othello"
    "demo-xasperate"
    "demo-unistrokes"
    "garnetdraw"
    "demos-controller"
    "tour"
    ))

(dolist (file Garnet-Demos-Files)
  (compile-file (user::garnet-pathnames file Garnet-Demos-Src))
  #+allegro-V3.1 (gc t))
