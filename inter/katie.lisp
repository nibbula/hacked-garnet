;;;
;;; katie.lisp - Keyboards And Things Intercepting Events
;;;

;; I have no idea what Katie was for, except that it seems to intercept
;; input events. Maybe recording input??
;; The backronym I made up is even more derpy than usual.

(in-package :inter)

(defun Katie-Button-Press (a-window x y state code event-key time)
  (declare (ignore a-window x y state code time))
  (format t "Katie says: Thanks for pressing ~a !!~%" event-key))

(defun Katie-Button-Release (a-window x y state code event-key time)
  (declare (ignore a-window x y state code time))
  (format t "Katie says: Thanks for releasing ~a !!~%" event-key))

(defun Katie-Motion-Notify (a-window x y display)
  (declare (ignore a-window display))
  (format t "Katie says: Thanks for moving to ~a ~a!!~%" x y))

(defun Katie-Key-Press (a-window x y state code time)
  (declare (ignore a-window x y state time))
  (format t "Katie says: Thanks for pressing the ~a key!!~%" code))

;; End
