;;;
;;; package.lisp - KR packages
;;;

(defpackage :kr-debug
  (:documentation "KR debugging.")
  (:use :common-lisp)
  ;; (:export)
  )

(defpackage :kr
  (:documentation "Constraint-Based Knowledge Representation.")
  (:use :common-lisp :kr-debug)
  (:export
   #:schema
   #:create-instance #:create-prototype #:create-relation #:create-schema
   #:formula #:o-formula
   #:schema-p #:relation-p #:is-a-p #:has-slot-p #:formula-p
   #:s-value #:g-value #:g-cached-value #:g-local-value #:gv #:gvl #:gv-local
   #:get-value #:get-local-value
   #:dovalues #:doslots
   #:define-method #:kr-send #:call-prototype-method #:apply-prototype-method
   #:method-trace
   #:with-constants-disabled #:with-types-disabled
   #:with-demons-disabled #:with-demon-disabled #:with-demon-enabled
   #:change-formula #:move-formula #:recompute-formula #:copy-formula #:kr-path
   #:mark-as-changed #:mark-as-invalid
   #:ps #:call-on-ps-slots #:name-for-schema
   #:declare-constant #:slot-constant-p
   #:destroy-slot #:destroy-schema #:destroy-constraint
   #:def-kr-type #:g-type #:s-type #:check-slot-type #:kr-boolean
   #:get-type-documentation #:set-type-documentation #:get-type-definition
   #:get-declarations #:get-slot-declarations
   #:g-formula-value #:s-formula-value
   ;; This should be exported but is not - LispWorks bug:
   ;; self-old-value

   ;; kr-doc
   #:get-slot-doc #:set-slot-doc
   
   ;; opal/types.lisp
   #:items-type #:accelerators-type #:known-as-type #:filename-type
   #:inter-window-type
   ))

;; End
