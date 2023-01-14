;;;
;;; package.lisp - Opal package
;;;

(defpackage :opal
  (:documentation "OPAL - Object Programming Aggregate Layer
Grahpical objects for Garnet.")
  (:use :common-lisp :kr)
  (:export
   #:bottom #:right #:center-x #:center-y
   #:gv-bottom #:gv-right #:gv-center-x #:gv-center-y
   #:gv-center-x-is-center-of #:gv-center-y-is-center-of
   #:gv-right-is-left-of #:gv-bottom-is-top-of
   #:top-side #:left-side #:bottom-side #:right-side
   #:center #:set-center
   #:bounding-box #:set-bounding-box
   #:set-position #:set-size
   #:draw #:erase #:rotate
   #:initialize #:calculate-bounding-box #:point-in-gob
   #:halftone #:halftone-darker #:halftone-lighter
   #:halftone-image #:halftone-image-darker #:halftone-image-lighter
   #:read-image #:write-image
   #:add-component #:remove-component #:move-component
   #:add-components #:remove-components #:remove-all-components
   #:do-components #:do-all-components
   #:point-to-component #:point-to-leaf
   #:set-aggregate-hit-threshold
   #:update #:destroy #:destroy-me
   #:raise-window #:lower-window #:iconify-window #:deiconify-window
   #:zoom-window #:fullzoom-window
	    
   ;; Class names
   #:aggregate #:view-object #:graphical-object #:line #:rectangle
   #:roundtangle #:multipoint #:polyline #:polygon #:text #:bitmap #:arc #:oval
   #:circle #:arrowhead #:multi-text #:cursor-multi-text
	    
   #:line-style #:default-line-style #:filling-style #:default-filling-style
   #:font #:cursor-text #:graphic-quality #:font-from-file #:cursor-font
   #:arrow-cursor #:arrow-cursor-mask #:arrow-pair
   #:hourglass-cursor #:hourglass-cursor-mask #:hourglass-pair
   #:garbage-cursor #:garbage-cursor-mask #:garbage-pair
   #:with-hourglass-cursor #:with-cursor #:default-font
   #:display-info-display #:display-info-screen
   #:display-info-root-window #:display-info-line-style-gc
   #:display-info-filling-style-gc
   #:convert-coordinates #:get-cursor-index #:string-width #:string-height
   #:change-cursors #:restore-cursors #:char-width
   #:move-cursor-down-one-line
   #:move-cursor-up-one-line
   #:move-cursor-to-beginning-of-line
   #:move-cursor-to-end-of-line
	    
   #:Get-X-Cut-Buffer #:Set-X-Cut-Buffer	;; for interactors' use
   #:leaf-objects-in-rectangle #:components-in-rectangle #:obj-in-rectangle
	    
   ;; filling and line style constants
   #:no-fill #:black-fill #:white-fill
   #:gray-fill #:light-gray-fill #:dark-gray-fill
   #:red-fill #:green-fill #:blue-fill #:yellow-fill
   #:cyan-fill #:orange-fill #:purple-fill
   #:motif-gray-fill #:motif-blue-fill #:motif-orange-fill #:motif-green-fill
   #:motif-light-gray-fill #:motif-light-blue-fill #:motif-light-orange-fill
   #:motif-light-green-fill
	    
   #:make-filling-style
   #:diamond-fill
   
   #:no-line #:thin-line #:line-0 #:line-1 #:line-2 #:line-4 #:line-8 #:gray-line
   #:dotted-line #:dashed-line 
   #:red-line #:green-line #:blue-line #:yellow-line
   #:cyan-line #:orange-line #:purple-line #:white-line
   
   ;; size of screen
   #:*screen-width* #:*screen-height*
   
   ;; Colors
   #:color #:white #:black #:red #:green #:blue #:cyan #:yellow #:orange #:purple
   #:motif-gray #:motif-blue #:motif-orange #:motif-green #:motif-light-gray
   #:motif-light-blue #:motif-light-orange #:motif-light-green
   
   ;; From Clean-Up.Lisp
   #:clean-up #:change-garnet-display #:update-all #:reset-cursor
   
   ;; From open-and-close.lisp
   #:disconnect-garnet #:reconnect-garnet
   
   ;; From process.lisp
   #:launch-main-event-loop-process
   #:kill-main-event-loop-process
   #:main-event-loop-process-running-p
   #:running-main-event-loop-process-elsewhere-p
   
   ;; From virtual-aggregates.lisp
   #:virtual-aggregate #:remove-item #:add-item #:change-item #:point-to-rank
   #:recalculate-virtual-aggregate-bboxes #:do-in-clip-rect
   
   #:get-standard-font

   ;; utils.lisp
   #:shell-exec #:make-image #:get-garnet-bitmap #:directory-p
   #:time-to-string #:clip-and-map #:drawable-to-window

   ;; pixmaps.lisp
   #:pixmap #:write-xpm-file #:read-xpm-file
   #:create-pixmap-image #:window-to-pixmap-image

   ;; ps
   #:Make-PS-File

   ;; aggregadgets
   #:gadget-add-item #:gadget-remove-item
   #:Set-Rank-Slots
   #:Single-Button-Get-Label #:Panel-Get-Label
   #:add-local-component #:add-local-interactor #:remove-local-component
   #:remove-local-interactor #:add-local-item
   #:remove-local-item #:remove-nth-item #:remove-nth-component
   #:notice-items-changed #:add-interactor #:remove-interactor
   #:take-default-component #:replace-item-prototype-object
   #:no-func
   #:GVL-SIBLING #:AGGREGADGET
   #:AGGREGRAPH #:AGGREGRAPH-NODE-PROTOTYPE #:AGGREGRAPH-LINK-PROTOTYPE
   #:LAYOUT-TREE
   #:source-to-graph-node #:remove-root #:make-root #:add-node #:layout-graph
   #:aggrelist #:null-object
   #:copy-gadget
   #:write-gadget #:e-formula #:*verbose-write-gadget*
   #:*required-names* #:*standard-names* #:*defined-names*
   #:SCALABLE-AGGREGRAPH-IMAGE #:SCALABLE-AGGREGRAPH-IMAGE-NODE-PROTOTYPE
   #:SCALABLE-AGGREGRAPH-IMAGE-LINK-PROTOTYPE
   #:SCALABLE-AGGREGRAPH #:SCALABLE-AGGREGRAPH-NODE-PROTOTYPE
   #:SCALABLE-AGGREGRAPH-LINK-PROTOTYPE
   ))
(in-package :opal)

;; End
