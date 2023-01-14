;;;
;;; event-receiver.lisp - Event receiver
;;;

(in-package :gem)

;; @@@ This relatively pointless class could be removed if the event loop was
;; redesigned to pull generic events from GEM, and not GEM calling into
;; Interactor event handlers. In other words make the event types generic and
;; make most the distribution happen in the device independent Interactor code.
;; Currently it just encapsulates things we need from the Interactor package.

(defclass event-receiver ()
  ((left-button   :initarg :left-button   :accessor left-button)
   (middle-button :initarg :middle-button :accessor middle-button)
   (right-button  :initarg :right-button  :accessor right-button)
   (double-click-time :initarg :double-click-time :accessor double-click-time
    :documentation
    "Controls spacing between clicks in a multiple-click event for X
     in milleseconds.")
   (double-offset :initarg :double-offset :accessor double-offset
    :documentation "The amount to add to *xx-button to get double-xx
                    For example: (+ *left-button* *double-offset*)
                                 = *double-left-button*")
   (mouse-up-translations
    :initarg :mouse-up-translations :accessor mouse-up-translations)
   (mouse-down-translations
    :initarg :mouse-down-translations :accessor mouse-down-translations)
   (modifier-translations
    :initarg :modifier-translations :accessor modifier-translations)
   (keysym-translations
    :initarg :keysym-translations :accessor keysym-translations)

   (ignore-undefined-keys
    :initarg :ignore-undefined-keys :accessor ignore-undefined-keys
    :initform t :type boolean)
   (break-key :initarg :break-key :accessor break-key)
   (trans-from-file :initarg :trans-from-file :accessor trans-from-file)
   (katie-base-char :initarg :katie-base-char :accessor katie-base-char
    :initform nil))
  (:documentation "Interface from GEM driver to Interactors.
Interactor methods that receive GEM events."))

(defvar *event-receiver* nil
  "The current event-receiver.")

(defgeneric Queue-Timer-Event (receiver inter))
(defgeneric base-char-to-character (receiver base-char bits))
(defgeneric translate-mouse-character (receiver
				       root-window button-code
				       modifier-bits event-key))

;; Events
(defgeneric do-map-notify (receiver window))
(defgeneric do-unmap-notify (receiver window))
(defgeneric do-circulate-notify (receiver))
(defgeneric do-gravity-notify (receiver))
(defgeneric do-configure-notify (receiver window x y width height above-sibling))
(defgeneric do-exposure (receiver window x y width height count display))
(defgeneric do-key-press (receiver window x y state code time))
(defgeneric do-button-press (receiver window x y state code time event-key))
(defgeneric do-button-release (receiver window x y state code time event-key))
(defgeneric do-motion-notify (receiver window x y display))
(defgeneric do-enter-notify (receiver window x y time))
(defgeneric do-leave-notify (receiver window x y time))

;; End
