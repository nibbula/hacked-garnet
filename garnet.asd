;;;								-*- Lisp -*-
;;; garnet.asd - System definition for garnet
;;;

(defsystem garnet
    :name               "garnet"
    :description
    "Generating an Amalgam of Real-time, Novel Editors and Toolkits."
    :version            "0.1.0"
    :author             "Garnet Authors"
    :license            "Unlicense"
    :source-control	:git
    :long-description
    "Garnet stands for Generating an Amalgam of Real-time, Novel Editors and 
Toolkits. It was originally developed by the User Interface Software Group 
in the Human Computer Interaction Institute in the School of Computer Science 
at Carnegie Mellon University in the early to mid 1990s. It is an environment 
for creating interfaces to Common Lisp software."
    :depends-on (:clx)
    :serial t
    :components
    ((:file "config")
     (:module "utils"
      :serial t
      :components ((:file "general")))
     (:module "kr"
      :serial t
      :components ((:file "package")
		   (:file "kr-macros")
		   (:file "kr")
		   (:file "constraints")))
     (:module "gem"
      :serial t
      :components ((:file "gem")
                   (:file "define-methods")
		   (:file "event-receiver")))
     (:module "opal"
      :serial t
      :components ((:file "package")
		   (:file "types")
                   (:file "update-constants")
                   (:file "defs")
                   (:file "macros")
                   (:file "new-defs")
                   (:file "utils")
                   (:file "text-fonts")
                   (:file "create-instances")
                   (:file "create-instances2")
                   (:file "text-functions")
                   (:file "text")
		   ;;
                   (:file "update-basics")
                   (:file "halftones")
                   (:file "objects")
                   (:file "roundtangles")
                   (:file "basics")
                   (:file "aggregates")
                   (:file "process")
                   (:file "clean-up")
                   (:file "windows")
                   (:file "update")
                   (:file "fast-redraw")
                   (:file "update-window")
                   (:file "virtual-aggregates")
                   (:file "pixmaps")
                   (:file "open-and-close")
		   ))
     (:module "gem-x"
      :serial t
      :pathname "gem"
      ;; :if-feature :clx
      :components ((:file "x")
		   (:file "x-inter")))
     #|
     (:module "gem-mac"
      :serial t
      :pathname "gem"
      if-feature :old-mac
      :components ((:file "mac")
		   (:file "mac-inter")))
     |#
     (:module "inter"
      :serial t
      :components ((:file "package")
                   (:file "interactors")		   
                   (:file "garnet-keytrans")
                   (:file "define-mouse-keys")
		   (:file "x-define-keys")   ;; #-apple
		   ;; (:file "mac-define-keys") ;; #+apple
		   (:file "katie")
                   (:file "accelerators")
                   (:file "animation-process")
                   (:file "i-windows")
                   (:file "menuinter")
                   (:file "movegrowinter")
                   (:file "buttoninter")
                   (:file "twopointinter")
                   (:file "textkeyhandling")
                   (:file "textinter")
                   (:file "angleinter")
                   (:file "animatorinter")
		   ))
     #|
     (:module "multifont" ;; @@@ see multifont/README
      :serial t
      :components ((:file "multifont")
                   (:file "lispkeyhandling")
                   (:file "multifont-textinter")
		   (:file "focus-multifont-textinter")
                   (:file "selection-interactor")
                   (:file "ps-multifont")
                   (:file "scrolling-window-multifont")))
     |#
     (:module "gesture"
      :serial t
      :components ((:file "package")
	           (:file "classify")
		   (:file "gestureinter")
	           (:file "features")
	           (:file "fileio")
	           (:file "matrix")))
     (:module "ps"
      :serial t
      :components ((:file "ps")
	           ;; (:file "ps-multifont") ;; see multifont/README
		   ))
     (:module "aggregadgets"
      :serial t
      :components ((:file "agg-macros")
                   (:file "agg-utils")
                   (:file "aggregadgets")
                   (:file "aggrelists")
                   (:file "add-agg")
                   (:file "agg-fix-slots")
                   (:file "copy-agg")
                   (:file "save-agg")
                   (:file "string-edit")
                   (:file "agg-labels")))
     (:module "aggregraphs"
      :serial t
      :pathname "aggregadgets"
      :components ((:file "rectangle-conflict-object")
		   (:file "aggregraphs")
		   (:file "scalable-aggregraph")
		   (:file "scalable-aggregraph-image")))
     (:module "gadgets"
      :serial t
      :components ((:file "GAD-scroll-parts")
                   (:file "GAD-slider-parts")
                   (:file "GAD-v-arrows")
                   (:file "GAD-v-boxes")
                   (:file "GAD-h-arrows")
                   (:file "GAD-h-boxes")
                   (:file "v-scroll-bar")
                   (:file "h-scroll-bar")
                   (:file "v-slider")
                   (:file "h-slider")
                   (:file "trill-device")
                   (:file "GAD-button-parts")
                   (:file "x-buttons")
                   (:file "text-buttons")
                   (:file "radio-buttons")
                   (:file "error-gadget-utils")
                   (:file "error-gadget")
                   (:file "scrolling-menu")
                   (:file "scrolling-input-string")
                   (:file "scrolling-labeled-box")
                   (:file "gauge")
                   (:file "menu")
                   (:file "menubar-functions")
                   (:file "menubar")
                   (:file "labeled-box")
                   (:file "arrow-line")
                   (:file "graphics-selection")
                   (:file "option-button")
                   (:file "popup-menu-button")
                   (:file "save-load-functions")
                   (:file "save-gadget")
                   (:file "load-gadget")
                   (:file "browser-gadget")
                   (:file "polyline-functions")
                   (:file "polyline-creator")
                   (:file "multi-selection")
                   (:file "scrolling-window-parts")
                   (:file "scrolling-window")
                   (:file "standard-edit")
                   (:file "mouseline")
                   (:file "prop-value-gadgets")
                   (:file "prop-sheet")
                   (:file "prop-sheet-win")
                   (:file "motif-parts")
                   (:file "motif-v-scroll-bar")
                   (:file "motif-h-scroll-bar")
                   (:file "motif-slider")
                   (:file "motif-trill-device")
                   (:file "motif-text-buttons")
                   (:file "motif-check-buttons")
                   (:file "motif-radio-buttons")
                   (:file "motif-menu")
                   (:file "motif-gauge")
                   (:file "motif-scrolling-labeled-box")
                   (:file "motif-prop-sheet-win")
                   (:file "motif-scrolling-window")
                   (:file "motif-error-gadget")
                   (:file "motif-option-button")
                   (:file "motif-scrolling-menu")
                   (:file "motif-save-gadget")
                   (:file "motif-load-gadget")
                   (:file "motif-menubar")
                   (:file "multifont-gadget")))
     #|
     (:module "protected-eval"
      :serial t
      :components ((:file "scrolling-unlabeled-box")
		   (:file "prompter")
		   (:file "protected-eval")
		   (:file "protected-process")))
     (:module "debug"
      :serial t
      :components ((:file "debug-fns")
                   (:file "objsize")
                   (:file "inspector")
                   (:file "suggest-constants")))
     |#
     ;; (:file "standard-names")
     ))
