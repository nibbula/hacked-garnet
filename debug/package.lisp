;;;
;;; package.lisp - Garnet debug package definition.
;;;

(defpackage :garnet-debug
  (:documentation "Garnet debug package definition.")
  (:use :cl :kr :opal)
  (:nicknames :gd)
  (:export
   ;; debug-fns.lisp
   #:explain-short #:explain-slot #:explain-nil
   #:fix-up-window #:flash #:ident #:invert 
   #:is-a-tree #:kids #:look #:look-inter 
   #:uninvert #:what #:where #:apple #:windows #:break-on-slot-set
   #:notify-on-slot-set #:clear-slot-set #:call-func-on-slot-set
   ;; inspector.lisp
   #:inspector #:inspect-next-inter #:Find-Slot-Starting-With
   #:*inspector-key* #:*show-object-key* #:*inspector-next-inter-key*
   ;; objsize.lisp
   #:objbytes #:aggbytes #:interbytes
   #:*avoid-shared-values* #:*avoid-equal-values* #:*count-symbols*
   #:count-formulas #:why-not-constant
   ;; suggest-constants.lisp
   #:record-from-now #:suggest-constants #:find-formulas #:explain-formulas
   ))

;; End
