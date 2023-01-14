;;;
;;; package.lisp - Interactors package
;;;

(defpackage :interactors
  (:documentation "Interactors")
  (:use :cl :kr)
  (:nicknames :inter)
  (:export
   #:*garnet-break-key*
   #:*left-button*
   #:*trans-from-file*

   ;; for animation
   #:start-animator #:Stop-Animator #:abort-animator #:animator-interactor
   #:animator-wrap #:animator-bounce #:Reset-All-Timer-Processes
   ;; entering and leaving main event loop
   #:main-event-loop #:exit-main-event-loop #:*garnet-break-key*
   ;; waiting for an interaction to complete
   #:Interaction-Complete #:Wait-Interaction-Complete
   ;;explicit control of interactors
   #:Change-Active #:Start-Interactor #:Abort-Interactor #:Stop-Interactor
   ;; Called by KR when :active or :window changes:
   #:Notice-Interactor-Slot-Changed
   ;; support for multiple priority levels
   #:priority-level #:normal-priority-level #:high-priority-level
   #:running-priority-level #:priority-level-list 
   ;; the next ones are for debugging
   #:Reset-Inter-Levels #:Print-Inter-Levels #:Print-Inter-Windows
   #:trace-inter #:untrace-inter #:*debug-next-inter* #:Do-All-Interactors
   ;; interactor event structure (copy of X's event structure)
   #:*Current-Event* #:*Garnet-Break-Key*
   #:event-x #:event-y #:event-char #:event-code #:event-mousep
   #:event-downp #:event-window #:event-timestamp #:make-event
   ;; for controlling double clicks
   #:*double-click-time*
   ;; key translations for text-inter
   #:Bind-Key #:Unbind-Key #:Unbind-All-Keys #:Set-Default-Key-Translations
   ;;transcripting functions
   #:Transcript-Events-To-File #:Close-Transcript
   #:Transcript-Events-From-File
   ;; useful utility functions
   #:Clip-And-Map #:Beep #:Insert-Text-Into-String #:Warp-Pointer
   #:Pop-Up-Win-And-Start-Interactor
   ;; functions for dealing with selection for button and menu
   #:Return-Final-Selection-Objs #:DeSelectObj #:SelectObj
   ;; the various exported interactor types
   #:interactor #:interactor-window #:button-interactor #:text-interactor
   #:two-point-interactor #:move-grow-interactor #:menu-interactor
   #:angle-interactor

   ;; accelerators
   #:*global-accelerators*	; defined in interactors.lisp
   #:*global-first-accelerators*	; defined in interactors.lisp
   #:*default-global-accelerators*

   #:add-global-accelerator      #:add-window-accelerator
   #:remove-global-accelerator   #:remove-window-accelerator
   #:clear-global-accelerators   #:clear-window-accelerators
   #:default-global-accelerators

   ;; Things used by other Interactors.
   #:Check-Interactor-Type
   #:Check-Required-Slots
   #:Set-Up-Defaults
   #:Fix-Running-Where
   #:GoToRunningState
   #:GoToStartState
   #:if-debug
   ))
(in-package :interactors)

(defparameter *num-modifier-keys* 4) 
(defparameter *num-mouse-buttons* 6) ;;3 buttons * 2 (for double-click
(defparameter *mouse-translation-dimensions*
  (list  (1+ *num-mouse-buttons*) (* *num-modifier-keys* *num-modifier-keys*)))

(defclass interactor-receiver (gem:event-receiver)
  ()
  (:default-initargs
   :left-button 1
   :middle-button 2
   :right-button 3
   :double-click-time 250
   :ignore-undefined-keys t
   :mouse-up-translations (make-array *mouse-translation-dimensions*)
   :mouse-down-translations (make-array *mouse-translation-dimensions*)
   :modifier-translations '()
   :keysym-translations (make-hash-table)
   )
  (:documentation "A bogus workaround."))

(defmethod translate-mouse-character ((receiver interactor-receiver)
				      root-window button-code modifier-bits
                                      event-key)
  (declare (ignore root-window))
  (case event-key
    (:button-release
     (aref inter::*mouse-up-translations*  button-code
           (inter::modifier-index modifier-bits)))
    (:button-press
     (aref inter::*mouse-down-translations*  button-code
           (inter::modifier-index modifier-bits)))))

;; Events
#| @@@ actually in i-windows.lisp
@@@ to make it extra ridiculous these mostly just call into opal

(defmethod do-map-notify ((receiver interactor-receiver)
			  window))
(defmethod do-unmap-notify ((receiver interactor-receiver)
			    window))
(defmethod do-circulate-notify ((receiver interactor-receiver))
(defmethod do-gravity-notify ((receiver interactor-receiver))
(defmethod do-configure-notify ((receiver interactor-receiver)
				window x y width height above-sibling))
(defmethod do-exposure ((receiver interactor-receiver)
			window x y width height count display))
(defmethod do-key-press ((receiver interactor-receiver)
			 window x y state code time))
(defmethod do-button-press ((receiver interactor-receiver)
			    window x y state code time event-key))
(defmethod do-button-release ((receiver interactor-receiver)
			      window x y state code time event-key))
(defmethod do-motion-notify ((receiver interactor-receiver)
			     window x y display))
(defmethod do-enter-notify ((receiver interactor-receiver)
			    window x y time))
(defmethod do-leave-notify ((receiver interactor-receiver)
			    window x y time))
|#

;; @@@ make the stupid receiver object so the other junk can work
(setf gem:*event-receiver* (make-instance 'interactor-receiver))

;; End
