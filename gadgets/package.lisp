;;;
;;; package.lisp - Garnet gadgets package
;;;

(defpackage :garnet-gadgets 
  (:documentation "Garnet gadgets package.")
  (:use :common-lisp :kr)
  (:nicknames :gg)
  (:export
   ;; #:load-gadget
   ;; arrow-line.lisp
   #:Arrow-Line #:Double-Arrow-Line
   #:arrow-line-go #:arrow-line-stop #:arrow-line-win #:arrow-line-agg
   ;; browser-gadget.lisp
   #:BROWSER-GADGET
   #:BROWSER-MENU-FN #:PUSH-FIRST-ITEM #:PROMOTE-ITEM
   #:SET-FIRST-ITEM #:BROWSER-MENU-SCROLL-FN #:BROWSER-SCROLL-FN
   ;; error-gadget.lisp
   #:ERROR-GADGET #:QUERY-GADGET
   #:Error-Gadget-Go #:Error-Gadget-Stop #:EGAGDET #:QGADGET
   ;; error-gadget-utils.lisp
   #:Display-Error #:Display-Error-And-Wait
   #:Display-Query #:Display-Query-And-Wait
   #:Careful-Eval #:Careful-String-Eval #:Careful-Read-From-String
   #:Careful-Eval-Formula-Lambda
   ;; guage.lisp
   #:Gauge
   #:Gauge-Go #:Gauge-Stop #:Gauge-Obj #:Gauge-Top-Agg #:Gauge-Win
   ;; graphics-selection.lisp
   #:Graphics-Selection
   ;; h-scroll-bar.lisp
   #:H-Scroll-Bar
   #:H-Scroll-Go #:H-Scroll-Stop
   #:H-Scroll-Win #:H-Scroll-Top-Agg #:H-Scroll-Obj
   ;; h-slider.lisp
   #:H-Slider
   #:H-Slider-Go #:H-Slider-Stop
   #:H-Slider-Win #:H-Slider-Top-Agg #:H-Slider-Obj
   ;; labeled-box.lisp
   #:Labeled-Box
   #:Labeled-Box-Go #:Labeled-Box-Stop
   #:Labeled-Box-Win #:Labeled-Box-Top-Agg #:Labeled-Box-Obj
   ;; load-gadget.lisp
   #:LOAD-GADGET
   ;; menubar-functions.lisp
   #:Add-Submenu-Item #:Remove-Submenu-Item
   #:Menubar-Components #:Submenu-Components #:Get-Submenu-Component
   #:Find-Submenu-Component #:Get-Bar-Component #:Set-Menubar #:Set-Submenu
   #:Menubar-Enable-Component #:Menubar-Disable-Component
   #:Menubar-Enabled-P #:Menubar-Installed-P
   #:Menubar-Get-Title #:Menubar-Set-Title
   ;; menubar.lisp
   #:MENUBAR #:BAR-ITEM #:SUBMENU #:SUBMENU-ITEM #:MAKE-SUBMENU-WIN
   #:Make-Menubar #:Make-Bar-Item #:Make-Submenu-Item
   #:MENUBAR-GO #:MENUBAR-STOP #:DEMO-MENUBAR #:MENUBAR-WIN #:MENUBAR-TOP-AGG
   ;; menu.lisp
   #:Menu
   #:Menu-Go #:Menu-Stop #:Menu-Obj #:Menu-Top-Agg #:Menu-Win
   ;; motif-check-buttons.lisp
   #:Motif-Check-Button #:Motif-Check-Button-Panel
   #:Motif-Check-Buttons-Go #:Motif-Check-Buttons-Stop
   #:Demo-Motif-Check-Button #:Demo-Motif-Check-Button-Panel
   #:Motif-Check-Buttons-Top-Agg #:Motif-Check-Buttons-Win
   #:Demo-Motif-Check-Button2
   ;; motif-error-gadget.lisp
   #:MOTIF-ERROR-GADGET #:MOTIF-QUERY-GADGET
   #:Motif-Error-Gadget-Go #:Motif-Error-Gadget-Stop #:MOTIF-EGADGET
   #:MOTIF-QGADGET
   ;; motif-gauge.lisp
   #:Motif-Gauge
   #:Motif-Gauge-Go #:Motif-Gauge-Stop
   #:Motif-Gauge-Win #:Motif-Gauge-Top-Agg #:Demo-Motif-Gauge
   ;; motif-h-scroll-bar.lisp
   #:Motif-H-Scroll-Bar
   #:Motif-H-Scroll-Go #:Motif-H-Scroll-Stop
   #:Demo-Motif-H-Scroll-Bar #:Motif-H-Scroll-Win #:Motif-H-Scroll-Top-Agg
   ;; motif-load-gadget.lisp
   #:MOTIF-LOAD-GADGET
   ;; motfi-menubar.lisp
   #:MOTIF-MENUBAR #:MOTIF-BAR-ITEM #:MAKE-MOTIF-SUBMENU-WIN
   #:Make-Motif-Menubar #:Make-Motif-Bar-Item #:Make-Motif-Submenu-Item
   #:MOTIF-MENUBAR-GO #:MOTIF-MENUBAR-STOP #:DEMO-MOTIF-MENUBAR
   #:MOTIF-MENUBAR-WIN #:MOTIF-MENUBAR-TOP-AGG
   ;; motif-menu.lisp
   #:Motif-Menu #:Motif-Menu-Accelerator-Inter
   #:Motif-Menu-Go #:Motif-Menu-Stop
   #:Demo-Motif-Menu #:Motif-Menu-Win #:Motif-Menu-Top-Agg
   ;; motif-option-button.lisp
   #:motif-option-button #:motif-option-button-go #:motif-option-button-stop
   ;; motif-parts.lisp
   #:MOTIF-TAB-PRIORITY-LEVEL #:MOTIF-TAB-INTER #:MOTIF-MENU-INTER
   #:MOTIF-BACKGROUND #:MOTIF-RECT
   ;; motif-prop-sheet-win.lisp
   #:motif-prop-sheet-with-OK #:motif-prop-sheet-for-obj-with-OK
   #:Motif-Prop-Sheet-For-Obj-With-Done
   #:Pop-Up-Win-For-Prop #:Pop-Up-Win-Change-Obj #:Pop-Up-Win-Change-Items
   ;; motif-radio-buttons.lisp
   #:Motif-Radio-Button #:Motif-Radio-Button-Panel
   #:Motif-Radio-Buttons-Go #:Motif-Radio-Buttons-Stop
   #:Demo-Motif-Radio-Button #:Demo-Motif-Radio-Button-Panel
   #:Demo-Motif-Radio-Button2 #:Motif-Radio-Buttons-Top-Agg
   #:Motif-Radio-Buttons-Win
   ;; motif-save-gadget.lisp
   #:MOTIF-SAVE-GADGET
   ;; motif-scrolling-labeled-box.lisp
   #:Insert-Text-Into-Box #:Motif-Scrolling-Labeled-Box
   #:Motif-Scrolling-Labeled-Box-Go #:Motif-Scrolling-Labeled-Box-Stop
   #:Motif-Scrolling-Labeled-Box-Win #:Motif-Scrolling-Labeled-Box-Top-Agg
   #:Demo-Motif-Scrolling-Labeled-Box
   ;; motif-scrolling-menu.lisp
   #:Motif-Scrolling-Menu
   #:Motif-Scrolling-Menu-Go #:Motif-Scrolling-Menu-Stop
   #:Motif-Scrolling-Menu-Obj #:Motif-Scrolling-Menu-Win
   #:Motif-Scrolling-Menu-Top-Agg
   ;; motif-scrolling-window.lisp
   #:Motif-Scrolling-Window-With-Bars
   #:Motif-Scrolling-Window-With-Bars-Go 
   #:Motif-Scrolling-Window-With-Bars-Stop
   ;; motif-slider.lisp
   #:Motif-Slider
   #:Motif-Slider-Go #:Motif-Slider-Stop
   #:Demo-Motif-Slider #:Motif-Slider-Win #:Motif-Slider-Top-Agg
   ;; motif-text-buttons.lisp
   #:Motif-Text-Button #:Motif-Text-Button-Panel
   #:Motif-Text-Buttons-Go #:Motif-Text-Buttons-Stop
   #:Demo-Motif-Text-Button #:Demo-Motif-Text-Button-Panel
   #:Motif-Text-Buttons-Top-Agg #:Motif-Text-Buttons-Win
   ;; motif-trill-device.lisp
   #:Motif-Trill-Device
   #:Motif-Trill-Win #:Motif-Trill-Agg #:Motif-Trill-Obj
   #:Motif-Trill-Go #:Motif-Trill-Stop
   ;; motif-v-scroll-bar.lisp
   #:Motif-V-Scroll-Bar
   #:Motif-V-Scroll-Go #:Motif-V-Scroll-Stop
   #:Demo-Motif-V-Scroll-Bar #:Motif-V-Scroll-Win #:Motif-V-Scroll-Top-Agg
   ;; mouseline.lisp
   #:MouseLine #:MouseLinePopup
   #:Mouseline-Go #:Mouseline-Stop
   ;; multifont-gadget.lisp
   #:Multifont-Gadget
   #:multifont-gadget-go #:multifont-gadget-stop
   ;; multi-selection.lisp
   #:Multi-Graphics-Selection #:Set-Selection #:Undo-Last-Move-Grow
   #:Multi-Graphics-Selection-Go
   #:Multi-Graphics-Selection-Obj
   #:Multi-Graphics-Selection-Stop
   ;; option-button.lisp
   #:Option-Button #:Option-Button-Go #:Option-Button-Stop
   ;; polyline-creator.lisp
   #:Polyline-Creator #:Stop-Polyline-Creator #:Abort-Polyline-Creator
   #:Toggle-Polyline-Handles #:Destroy-Polyline-Handles
   #:Hide-Polyline-Handles
   #:Polyline-Creator-demo-go #:Polyline-Creator-demo-stop
   ;; popup-menu-button.lisp
   #:Popup-Menu-Button #:lines-bitmap #:downarrow-bitmap
   #:Popup-Menu-Button-Go #:Popup-Menu-Button-Stop
   ;; prop-sheet.lisp
   #:Prop-Sheet #:ReUsePropSheet #:prop-sheet-for-obj
   #:ReUsePropSheetObj #:Get-Val-For-PropSheet-Value
   #:Set-Val-For-PropSheet-Value
   #:prop-sheet-for-obj-Go  #:prop-sheet-for-obj-stop
   ;; prop-sheel-win.lisp
   #:prop-sheet-with-OK #:prop-sheet-for-obj-with-OK #:Pop-Up-Win-For-Prop
   #:Pop-Up-Win-Change-Obj #:Pop-Up-Win-Change-Items
   ;; prop-value-gadgets.lisp
   #:Horiz-Choice-List #:Pop-Up-From-Icon
   ;; radio-buttons.lisp
   #:Radio-Button #:Radio-Button-Panel
   #:Radio-Buttons-Go #:Radio-Buttons-Stop #:Radio-Button-Obj
   #:Radio-Buttons-Obj #:Radio-Buttons-Top-Agg #:Radio-Buttons-Win
   #:Radio-Button-Obj2
   ;; save-gadget.lisp
   #:SAVE-GADGET
   ;; save-load-functions.lisp
   #:DISPLAY-SAVE-GADGET #:DESTROY-SAVE-GADGET #:HIDE-SAVE-GADGET
   #:DISPLAY-LOAD-GADGET #:DESTROY-LOAD-GADGET #:HIDE-LOAD-GADGET
   #:display-save-gadget-and-wait #:display-load-gadget-and-wait
   #:save-file-if-wanted
   ;; scrolling-input-string.lisp
   #:Scrolling-Input-String #:Insert-Text-Into-Box #:Insert-Text-Into-SIS
   #:Scrolling-Input-String-Go #:Scrolling-Input-String-Stop
   #:Scrolling-Input-String-Top-Agg #:Scrolling-Input-String-Win
   #:Scrolling-Input-String-Obj
   ;; scrolling-labeled-box.lisp
   #:Scrolling-Labeled-Box #:Scrolling-Labeled-Box-Win
   #:Scrolling-Labeled-Box-Go #:Scrolling-Labeled-Box-Stop
   #:Scrolling-Labeled-Box-Obj
   ;; scrolling-menu.lisp
   #:Scrolling-Menu
   #:Scrolling-Menu-Go #:Scrolling-Menu-Stop
   #:Scrolling-Menu-Obj #:Scrolling-Menu-Win #:Scrolling-Menu-Top-Agg
   ;; scrolling-window-lisp
   #:Scrolling-Window-With-Bars
   #:Scrolling-Window-With-Bars-Go #:Scrolling-Window-With-Bars-Stop
   ;; scrolling-window-parts.lisp
   #:Scrolling-Window #:Auto-Scroll
   #:Scroll-Win-Inc #:Scroll-Win-To
   #:Scrolling-Window-Go #:Scrolling-Window-Stop
   #:Show-Box #:Show-Cursor
   ;; standard-edit.lisp
   #:Standard-Delete #:Standard-Delete-All #:Standard-Undo-Last-Delete
   #:Standard-To-Bottom #:Standard-To-Top
   #:Standard-Duplicate #:Standard-Copy #:Standard-Cut #:Standard-Select-All
   #:Standard-Refresh
   #:Standard-Paste-Same-Place #:Standard-Paste-Inc-Place
   #:Standard-Initialize-Gadget #:Standard-NIY
   #:Standard-Group #:Standard-UnGroup
   #:Clipboard-Object
   #:sort-objs-display-order #:Is-A-Motif-Background #:Is-A-Motif-Rect
   ;; text-buttons.lisp
   #:Text-Button #:Text-Button-Panel
   #:Text-Buttons-Go #:Text-Buttons-Stop #:Text-Button-Obj
   #:Text-Buttons-Obj #:Text-Buttons-Top-Agg #:Text-Buttons-Win
   #:Text-Button-Obj1
   ;; trill-device.lisp
   #:Trill-Device
   #:Trill-Go #:Trill-Stop
   #:Trill-Top-Agg #:Trill-Win #:Trill-Obj
   ;; v-scroll-bar.lisp
   #:V-Scroll-Bar
   #:V-Scroll-Go #:V-Scroll-Stop
   #:V-Scroll-Obj #:V-Scroll-Win #:V-Scroll-Top-Agg
   ;; v-slider.lisp
   #:V-Slider
   #:V-Slider-Go #:V-Slider-Stop
   #:V-Slider-Win #:V-Slider-Top-Agg #:V-Slider-Obj
   ;; x-buttons.lisp
   #:X-Button #:X-Button-Panel
   #:X-Buttons-Go #:X-Buttons-Stop
   #:X-Buttons-Obj #:X-Button-Obj #:X-Buttons-Top-Agg #:X-Buttons-Win
   #:X-Button-Obj2
   ;; contrib/prompter/scrolling-unlabeled-box.lisp
   #:Scrolling-Unlabeled-Box
   #:Scrolling-Unlabeled-Box-Go #:Scrolling-Unlabeled-Box-Stop
   #:Scrolling-Unlabeled-Box-Obj
   ;; protected-eval/protected-eval.lisp
   #:protected-eval-error-gadget
   #:garnet-error-handler #:garnet-user-error-hander
   #:with-garnet-error-handling #:with-garnet-user-error-handling
   #:garnet-protected-eval #:garnet-protected-read-from-string
   #:garnet-protected-read #:do-prompt #:error-prompter-gadget
   #:with-normal-cursor #:*normal-cursor-pair*
   ;; protected-eval/prompter.lisp
   #:prompter-gadget #:display-prompt #:display-prompt-and-wait
   #:prompter-gadget-go #:prompter-gadget-stop
  ))
(in-package :garnet-gadgets)

;; @@@ Maybe should these should be in a types.lisp ?

(def-kr-type '(OR NULL STRING KEYWORD (SATISFIES SCHEMA-P)))

(def-kr-type '(OR STRING KEYWORD (SATISFIES SCHEMA-P)))

(def-kr-type '(OR KEYWORD CHARACTER LIST))

(def-kr-type '(OR NULL KEYWORD CHARACTER))

(def-kr-type '(SATISFIES CHECK-MENUBAR-ITEMS))

;; End
