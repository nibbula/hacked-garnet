;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: GARNET-UTILS; Base: 10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; general.lisp
;;
;; by David S. Kosbie

#|
============================================================
Change log:
       12/06/94 Bruno Haible - Named package in system::*error-handler*
        1/08/94 Andrew Mickish - Added PI variables
        9/22/93 Bruno Haible - Ignored args in Probe-Directory
        8/23/93 Andrew Mickish - Added probe-directory for CLISP
        7/01/93 Andrew Mickish - Removed optimization proclamation
        6/15/93 Andrew Mickish - Safe-functionp now checks whether the symbol
                  is fbound -- so you can supply symbols that refer to fns.
        6/10/93 Andrew Mickish - Moved safe-functionp here from aggrelists
        6/ 3/93 Andrew Mickish - Moved verify-binding here from demo-graph
                  and demo-schema-browser
        4/ 5/93 Dave Kosbie - created
============================================================
|#

(defpackage :garnet-utils
  (:documentation
   "This file defines a host of Lisp utilities used by other Garnet code.")
  (:use :common-lisp)
  (:nicknames :gu)
  (:export
   #:while
   #:until
   #:do2lists
   #:dolist2
   #:m
   #:m1
   #:string+
   #:add-to-list
   #:verify-binding
   #:safe-functionp
   #:probe-directory
   #:pi/2 #:pi3/2 #:2pi #:-2pi #:short-pi
   #:define-constant
   #:with-muffled-style-warnings
   ))
(in-package :garnet-utils)

(defconstant pi/2 (/ pi 2))
(defconstant pi3/2 (* 3 (/ pi 2)))
(defconstant 2PI (* 2 PI))
(defconstant -2PI (- (* 2 PI)))
(defconstant short-PI (coerce PI 'short-float))

(defmacro while (test &rest body)
  `(loop
     (if (not ,test) (return))
     ,@body))

(defmacro until (test &rest body)
  `(loop
     ,@body
     (if ,test (return))))

(defmacro do2lists ((var1 list1 var2 list2 &key either?) &rest body)
 (let ((list1var  (gensym))
       (list2var  (gensym))
       (done-test (if either? 'and 'or)))
  `(let ((,list1var ,list1)
	 (,list2var ,list2)
	 ,var1 ,var2)
      (while (,done-test ,list1var ,list2var)
	(setq ,var1 (car ,list1var))
	(setq ,var2 (car ,list2var))
	(setq ,list1var (cdr ,list1var))
	(setq ,list2var (cdr ,list2var))
       ,@body))))

(defmacro dolist2 ((var1 var2 list) &rest body)
  (let ((listvar (gensym)))
  `(let ((,listvar ,list) ,var1 ,var2)
     (while ,listvar
       (setq ,var1 (car ,listvar))
       (setq ,var2 (cadr ,listvar))
       (setq ,listvar (cddr ,listvar))
       ,@body))))

(defmacro m (s-expr)
  `(pprint (macroexpand (quote ,s-expr))))

(defmacro m1 (s-expr)
  `(pprint (macroexpand-1 (quote ,s-expr))))

(defmacro string+ (&rest args) `(concatenate 'string ,@args))

;; legal invocations:
;;  (add-to-list element list)
;;  (add-to-list element list :head) (or :front)
;;  (add-to-list element list :tail) (or :back)
;;  (add-to-list element list :before other-element)
;;  (add-to-list element list :after other-element)
(defun add-to-list (element list &optional where locator)
 (let  ((new-cons (list element))
	result)
 (if (null list)
  (setq result new-cons)
  (case where
	((:head :front NIL) (setq result (cons element list)))
	((:before)
		(if (eq (first list) locator)
		    (setq result (cons element list))
		    (do ((cons1 list       (cdr cons1))
		         (cons2 (cdr list) (cdr cons2)))
		        ((null cons2))
		        (when (eq (first cons2) locator)
		          (setf (cdr new-cons) cons2   )
		          (setf (cdr cons1   ) new-cons)
		          (setq result list)
		          (return)))))
	((:after)
		  (do ((cons1 list       (cdr cons1))
		       (cons2 (cdr list) (cdr cons2)))
		      ((null cons1))
		        (when (eq (first cons1) locator)
		          (setf (cdr new-cons) cons2   )
		          (setf (cdr cons1   ) new-cons)
		          (setq result list)
		          (return))))))
 (unless result
	(setf (cdr (last list)) new-cons)
	(setq result list))
 result))



;;;---------------------------------------------------------------------------
;;;  Verify-Binding
;;;

; Takes a string and returns the symbol coercion of the string if the
; symbol is bound.  Note: The suffix of the string is converted to all
; uppercase characters before checking if it is bound in the package.
(defun VERIFY-BINDING (string)
  (let ((result-1 (verify-binding-aux string 0)))
    (if result-1
	(let* ((colon-p (car result-1))
	       (prefix (cadr result-1))
	       (symbol-1 (values (read-from-string prefix)))
	       (index (caddr result-1)))
	  (if colon-p
	      ; Then symbol-1 indicates a package name
	      (if (find-package symbol-1)
		  ; Then symbol-1 is a valid package name
		  (let ((result-2 (verify-binding-aux string (+ 1 index))))
		    (if result-2
			; Then suffix indicates a var in the package symbol-1
			(let* ((suffix (string-upcase (cadr result-2)))
			       (access-internal-p (cadddr result-2)))
			  (multiple-value-call
			   #'(lambda (symbol-2 access)
			       (if symbol-2
				   (if (or (eq access :external)
					   access-internal-p)
				       ; verify that symbol-2 is not a function
				       (if (boundp symbol-2) 
					   (values (read-from-string string)))
				       )))
			   (find-symbol suffix symbol-1))))))
	      ; Then symbol indicates a var in the working package
	      (if (and (not (numberp symbol-1)) (boundp symbol-1)) symbol-1)
	      )))))


(defun VERIFY-BINDING-AUX (string start)
  (let ((str-len (length string))
	(colon-p NIL))
    (when (> str-len start)
      ; Skip second colon if there is a double colon between package and var
      (let ((access-internal-p (if (and (char= (char string start) #\:)
					(/= start 0))
				   (incf start))))
	; Abort if a special character begins the string
	(when (not (or (char= (char string start) #\:)
		       (char= (char string start) #\#)))
	  ; Return the part of the string up to but not including the colon
	  ; and the index of the last character checked
	  (do* ((i start (+ i 1))
		(current-char (char string i) (char string i))
		(new-string (string current-char)
			    (if (char= current-char #\:)
				(progn (setf colon-p T) new-string)
				(concatenate 'string
				 new-string (string current-char)))))
	       ((or colon-p (= i (- str-len 1)))
		(list colon-p new-string i access-internal-p))))))))

;;;
;;; (end) Verify-Binding
;;;---------------------------------------------------------------------------


;; Lucid thinks NIL and keywords are functions!
(defun safe-functionp (fn)
  (or (and fn (not (keywordp fn)) (functionp fn))
      (and (symbolp fn) (fboundp fn))))

(defun probe-directory (filename)
  #+clisp (let ((system::*error-handler*
		 #'(lambda (&rest args)
		     (declare (ignore args))
		     (return-from probe-directory nil))
		  ))
	    ; The following causes an error if the directory does not exist
	    (and (truename filename) t))
  #-clisp (probe-file filename)
  )

(defun same-with-warning (name new test)
  "If ‘name’ is bound, return it's value, but but warn if ‘new’ is different
from it according to ‘test’. Return ‘new’ if name isn't bound."
  (if (boundp name)
      (let ((old (symbol-value name)))
	(unless (funcall test old new)
	  (warn "Not redefining ~s from ~s to ~s." name old new))
	old)
      new))

(defmacro define-constant (name value &optional doc (test ''equal))
  "Like ‘defconstant’ but don't actually redefine the constant. If the ‘value’
is equal according to ‘test’, which defaults to ‘equal’, then don't even
complain. Otherwise just warn, and don't redefine it."
  `(cl:defconstant ,name (same-with-warning ',name ,value ,test)
     ,@(when doc (list doc))))

(defmacro with-muffled-style-warnings (&body body)
  "Muffle ‘style-warnings’ on some implementations."
  `(locally
       #+sbcl (declare (sb-ext:muffle-conditions style-warning))
       ,@body))
