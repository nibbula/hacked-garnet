;;;
;;; package.lisp - Gesture package definition.
;;;

(defpackage :gesture
  (:documentation "Gesture package definition.")
  (:use :cl :kr :inter)
  (:export
   #:gesture-interactor

   #:gest-classify                 ;; functions in classify.lisp
   #:gest-new-classifier
   #:make-gest-class
   #:gest-class-name
   #:gest-class-examples

   #:gest-attributes-minx          ;; functions in features.lisp 
   #:gest-attributes-maxx
   #:gest-attributes-miny
   #:gest-attributes-maxy
   #:gest-attributes-initial-sin    
   #:gest-attributes-initial-cos
   #:gest-attributes-startx     
   #:gest-attributes-starty
   #:gest-attributes-endx       
   #:gest-attributes-endy
   #:gest-attributes-dx2           
   #:gest-attributes-dy2          
   #:gest-attributes-magsq2      
   #:gest-attributes-path-r
   #:gest-attributes-path-th
   #:gest-attributes-abs-th
   #:gest-attributes-sharpness

   #:gest-classifier-read          ;; functions in fileio.lisp
   #:gest-classifier-write
   #:gest-classifier-convert

   #:gest-add-example		   ;; train.lisp
   #:gest-classifier-train
   #:gest-done-adding 
   #:gest-remove-example
   #:gest-strip-sumcov
   ))
(in-package :gesture)

;; End
