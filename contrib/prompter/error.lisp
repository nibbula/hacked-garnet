;;; -*- mode: fi:common-lisp; package: rga-utils -*-

;;; Copyright 1993 Russell G. Almond

;;; This code is in the Public Domain.  Anyone who can get some use
;;; from it is welcome.
;;; This code comes with no warentee.

(in-package :rga)

;;; Some useful error handling routines.

;;;
;;;----------------------------------------------------------------------
;;; Abort Restarts
;;;----------------------------------------------------------------------


(defun do-abort ()
  "Never Exits!  This turns control over to the restart abort.  If
not, it tries to return control to the top level.  This will in
general be system dependent and need to be hacked."
  (if (find-restart 'abort) (abort)	;Worked!
    #+(and allegro-version>= (version>= 4 1))
    (top-level:do-command :reset)
    #-(and allegro-version>= (version>= 4 1))
    (abort)				;this will generate error
					;which means you need to find
					;a solution for your lisp.
    ))



(defmacro with-abort (&body forms)
  "Executes forms in an environment in which there exists an abort
restart.  The abort restart returns two values, nil and :abort"
  (let ((abort-block-tag (gensym "ABORT")))
    `(block ,abort-block-tag
       (restart-case (progn ,.forms)
       (abort () :report "Abandon Computation, Return nil"
       (return-from ,abort-block-tag (values nil :abort)))))))


(defvar *user-type* :programmer
  "Set this to :programmer if the user should be allowed to enter the debugger.
  In protected evaluation mode."
  )

;;;
;;;----------------------------------------------------------------------
;;; Prompting Handler for valuation-type errors
;;;----------------------------------------------------------------------

(defun prompting-error-handler (context condition &key
						    (allow-debugger
						     (eql *user-type* :programmer)))
  (declare (type String context)
	   (type Condition condition))
  "Handles an error by prompting the user for abort, debug,
store-value, use-value or continue.  <allow-debugger> controls whether
the user is given the option of entering the debugger.

Note that this function takes two arguments, while hanlers are usually
meant to take one.  It should be invoked with
  (handler-bind ((error #'(lambda (condition)
                             (prompting-error-handler context-string condition))))
               ...)  or the like.

This function will eventually invoke a restart contindition and hence
should never return.

"
  (let* ((continue-restart (find-restart 'continue))
	 (store-restart (find-restart 'store-value))
	 (use-restart (find-restart 'use-value))
	 (option-list '((:Abort . "Stop calculations (return to top level)")))
	 (option nil))
    (when allow-debugger
      (setq option-list (cons '(:Debug . "Invoke LISP debugger.") option-list)))
    (when use-restart
      (setq option-list (cons (cons :use-value use-restart) option-list)))
    (when store-restart
      (setq option-list (cons (cons :store-value store-restart) option-list)))
    (when continue-restart
      (setq option-list (cons (cons :continue continue-restart) option-list)))
    (format *debug-io* "While ~A~& I generated the error~&~A~%"
	    context condition)
    (loop
      (format *debug-io* "~&Options for handling the error:~%")
      (dotimes (i (length option-list))
	(let ((option (nth i option-list)))
	  (format *debug-io* "~&~A.  ~S  ~A"
		  i (car option) (cdr option))))
      (format *debug-io* "~&Select an option by name or number.::~%")
      (setq option (read *debug-io*))
      (when (and (numberp option) (< option (length option-list)))
	(setq option (car (nth option option-list))))
      (case option
	(:ABORT (do-abort))
	(:DEBUG (when allow-debugger (invoke-debugger condition)))
	(t (setq option (find option option-list :key #'car))
	   (if option
	       (invoke-restart-interactively (cdr option)))))
      (format *debug-io* "~&Invalid option, please select one of:~%"))))




(defmacro with-prompting-error-handling (context &body forms)
  "Executes forms in a protected environment where errors are handled
by prompting-error-handler, which creates queries the user with
options to abort or continue, possibly with various recovery
strategies.  If rga:*user-type* is :programmer, then allows debugging.

<context> should be a string describing user meaningful context in
which error occured."
  `(handler-bind ((error ,#'(lambda (condition)
			      (prompting-error-handler context condition))))
     ,.forms))


(defun prompting-protected-eval (form &key (default-value nil dv?)
					   (context (format nil
							    "Evaluating ~S"
							    form))
					   (allow-debug (eq *user-type* :programmer))
					   (local-abort nil)
					   (abort-val nil))
  "This function executes a form in an environment where errors are
caught and handled by a special protected-error gadget.  This
gadget prints the error message and allows for several different
restarts:  ABORT, DEBUG and CONTINUE, USE-VALUE and STORE-VALUE.

<form> is the form to be evaluated.
<default-value> if supplied, produces a continue restart which returns
that value.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter.

If <allow-debug> is nil (defaul (eq *user-type* :programmer)) then the
debug switch is suppressed.

<context> is a string defining the context of the error.  Default
value is `Evaluating <form>'.
"

  (let* ((handler-function
	  #'(lambda (condition)
	      (prompting-error-handler context condition :allow-debugger
				    allow-debug)))
	 (handled-form `(handler-bind ((error ,handler-function))
			  ,form)))
    (when dv?
      (setq handled-form
	`(restart-case ,handled-form
	   (continue ()
	     :report (lambda (s)
		       (format s "Return value ~S" ,default-value))
	     ,default-value))))
    (when local-abort
      (setq handled-form
	`(restart-case ,handled-form
	   (abort ()
	     :report (lambda (s)
		       (format s "Abort and return value ~S" ,abort-val))
	     (values ,abort-val :abort)))))
    (eval handled-form)))


(defun prompting-protected-read
    (&optional (stream *standard-input*)
     &key (context (format nil "Reading from ~S" stream))
	  (read-package *package*)
	  (read-bindings nil)
	  (default-value nil)
	  (allow-debug nil)
	  (local-abort nil)
	  (abort-val nil))
  "This works rather like protected-eval except it tries to
read from the <stream>.

<stream> is the stream to be read from (if omitted *standard-input*).


<read-package> (default :user) selects the package to read from.  This
is because I don't want to make any assumptions about what the binding
of package will be at eval time especially in a multiprocessed lisp,
and I think this is safer.  If you want the string to be read in a
different package,  you can try using :read-package *package*

<read-bindings> is a list of (var . form)'s as in a let statement.
These bindings are made (with the let) before reading the string to
allow for effects such as binding the readtable.

<default-value> (default nil) this establishes a continue restart
which returns this value.  Note that this is slightly different from
protected-eval in that it is always available.

<allow-debug> (default (eq *user-type* :programmer) if true, this
includes a button which allows the debugger to be entered on an error.
Note that the default value is different from protected-eval.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter. (Same as
protected-eval).

"
  (let* ((form `(let ((*package* ,read-package))
		  (let ,read-bindings
		    (read ,stream))))
	 (handler-function
	  #'(lambda (condition)
	      (prompting-error-handler context condition
				       :allow-debugger allow-debug)))
	 (handled-form
	  `(handler-bind ((error ,handler-function))
	     ,form)))
    (when T
      (setq handled-form
	`(restart-case ,handled-form
	   (continue ()
	       :report (lambda (s)
			 (format s "Return value ~S" ,default-value))
	     ,default-value))))
    (when local-abort
      (setq handled-form
	`(restart-case ,handled-form
	   (abort ()
	       :report (lambda (s)
			 (format s "Return value ~S" ,abort-val))
	     (values ,abort-val :abort)))))
    (eval handled-form)))


(defun prompting-protected-read-from-string
    (string
     &key (start 0)
	  (end (length string))
	  (context (format nil "Parsing ~A" string))
	  (read-package *package*)
	  (read-bindings nil)
	  (default-value nil)
	  (allow-debug nil)
	  (local-abort nil)
	  (abort-val nil))
  "This works rather like protected-eval except it tries to
read from the <stream>.

<string> is the string to be read from (probably the :string of a text
input gadget).

<start> and <end> allow selecting a substring.

<read-package> (default :user) selects the package to read from.  This
is because I don't want to make any assumptions about what the binding
of package will be at eval time especially in a multiprocessed lisp,
and I think this is safer.  If you want the string to be read in a
different package,  you can try using :read-package *package*

<read-bindings> is a list of (var . form)'s as in a let statement.
These bindings are made (with the let) before reading the string to
allow for effects such as binding the readtable.

<default-value> (default nil) this establishes a continue restart
which returns this value.  Note that this is slightly different from
protected-eval in that it is always available.

<allow-debug> (default (eq *user-type* :programmer) if true, this
includes a button which allows the debugger to be entered on an error.
Note that the default value is different from protected-eval.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter. (Same as
protected-eval).

"
  (let* ((form `(let ((*package* ,read-package))
		  (let ,read-bindings
		    (read-from-string ,(subseq string start end)))))
	 (handler-function
	  #'(lambda (condition)
	      (prompting-error-handler context condition
				       :allow-debugger allow-debug)))
	 (handled-form
	  `(handler-bind ((error ,handler-function))
	     ,form)))
    (when T
      (setq handled-form
	`(restart-case ,handled-form
	   (continue ()
	       :report (lambda (s)
			 (format s "Return value ~S" ,default-value))
	     ,default-value))))
    (when local-abort
      (setq handled-form
	`(restart-case ,handled-form
	   (abort ()
	       :report (lambda (s)
			 (format s "Return value ~S" ,abort-val))
	     (values ,abort-val :abort)))))
    (eval handled-form)))



(defun prompter (prompt &optional (stream *query-io*)
		 &key (local-abort nil)
		      (default-value nil dv?)
		      (abort-val :ABORT)
		      (satisfy-test #'(lambda (obj) T))
		      (eval-input? nil)
		 &allow-other-keys
		 &aux flag form val test?)
  "Prompts user for an input.  <Prompt> is printed with ~A as a prompt.
<stream> defaults to *query-io*.  If <local-abort> is true a local
abort is set up which will return the values <abort-val> and :ABORT.
If <default-value> is supplied, a CONTINUE restart is set up which
allows the user to select the default value.

If <eval-input?> is true, then the expression is evaluated before it
is returned; if not, the unevaluated expression is returned.  

The value supplied by the user is passed to <satisfy-test>.  If that
test fails, the user is prompted again."
  (loop 
    (format stream "~A~%==>" prompt)
    (multiple-value-setq (form flag)
      (apply #'prompting-protected-read stream 
	     :local-abort local-abort :abort-val abort-val
	     (if dv? (list :default-value default-value) '())))
    (unless (eq flag :ABORT)
      (if eval-input?
	  (multiple-value-setq (val flag)
	    (apply #'prompting-protected-eval form 
		   :local-abort local-abort :abort-val abort-val
		   (if dv? (list :default-value default-value) '())))
	(setq val form)))
    (if (eq flag :ABORT)
	(if local-abort (return-from prompter (values abort-val :ABORT)))
      (progn
	(multiple-value-setq (test? flag)
	  (ignore-errors (funcall satisfy-test val)))
	(if test? (return-from prompter (values val :OK))
	  (if (typep flag 'Condition)
	      (format stream "~&Error: ~A~%" flag)))))
    (format stream "~&Bad Input, try again~%" prompt)))


;;;
;;;----------------------------------------------------------------------
;;; Abstract error handler functions
;;;----------------------------------------------------------------------
;;;
;;; These functions provide an abstraction of the error handling
;;; facilities which can be bound as appropriate.



(defun protect-errors (context condition &key
					 (allow-debugger
					  (eql *user-type* :programmer)))
  "Error handler which prompts user for a choice of
   :ABORT, :DEBUG, :CONTINE, :USE-VALUE and :STORE-VALUE
   restarts.

<context> is used to supply a context for the error.
<allow-debugger> is used to determine whether or not the user can
enter the LISP debugger.

Should be invoked with an expression such as:
  (handler-bind 
    ((error \#'(lambda (condition)
		 (protect-errors context-string condition))))
  ...)

"
  (prompting-error-handler context condition :allow-debugger allow-debugger))



(defmacro with-protected-errors (context &body forms)
  "Executes forms in a protected environment where errors are handled
by prompting-error-handler, which creates queries the user with
options to abort or continue, possibly with various recovery
strategies.  If rga:*user-type* is :programmer, then allows debugging.

<context> should be a string describing user meaningful context in
which error occured."
  `(handler-bind ((error ,#'(lambda (condition)
			      (protect-errors context condition))))
     ,.forms))


(defun protected-eval (form &rest args
		       &key (default-value nil dv?)
			    (context (format nil "Evaluating ~S" form))
			    (allow-debug (eq *user-type* :programmer))
			    (local-abort nil)
			    (abort-val nil))
  "This function executes a form in an environment where errors are
caught and handled by a special protected-error.  This
gadget prints the error message and allows for several different
restarts:  ABORT, DEBUG and CONTINUE, USE-VALUE and STORE-VALUE.

<form> is the form to be evaluated.
<default-value> if supplied, produces a continue restart which returns
that value.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter.

If <allow-debug> is nil (defaul (eq *user-type* :programmer)) then the
debug switch is suppressed.

<context> is a string defining the context of the error.  Default
value is `Evaluating <form>'.

This abtract function allows the type of error handler to be hidden
from the routine which sets it up.  In particular, both
promting-protected-eval and protected-eval could be bound to
this symbol."
  (apply #'prompting-protected-eval args))

(defun protected-read (&optional (stream *standard-input*)
				 &rest args
		       &key (context (format nil "Reading from ~S" stream))
			    (read-package *package*)
			    (read-bindings nil)
			    (default-value nil)
			    (allow-debug nil)
			    (local-abort nil)
			    (abort-val nil))
  "This works rather like protected-eval except it tries to
read from the <stream>.

<stream> is the stream to be read from (if omitted *standard-input*).

<read-package> (default :user) selects the package to read from.  This
is because I don't want to make any assumptions about what the binding
of package will be at eval time especially in a multiprocessed lisp,
and I think this is safer.  If you want the string to be read in a
different package,  you can try using :read-package *package*

<read-bindings> is a list of (var . form)'s as in a let statement.
These bindings are made (with the let) before reading the string to
allow for effects such as binding the readtable.

<default-value> (default nil) this establishes a continue restart
which returns this value.  Note that this is slightly different from
protected-eval in that it is always available.

<allow-debug> (default (eq *user-type* :programmer) if true, this
includes a button which allows the debugger to be entered on an error.
Note that the default value is different from protected-eval.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter. (Same as
protected-eval).

This abtract function allows the type of error handler to be hidden
from the routine which sets it up.  In particular, both
promting-protected-eval and protected-eval could be bound to
this symbol."
  (apply #'prompting-protected-read stream args))

(defun protected-read-from-string
    (string &rest args
     &key (start 0)
	  (context (format nil "Parsing ~S" string))
	  (end (length string))
	  (read-package *package*)
	  (read-bindings nil)
	  (default-value nil)
	  (allow-debug nil)
	  (local-abort nil)
	  (abort-val nil))
  "This works rather like protected-eval except it tries to
read from the <stream>.

<string> is the string to be read from (probably the :string of a text
input gadget).

<start> and <end> allow selecting a substring.

<read-package> (default :user) selects the package to read from.  This
is because I don't want to make any assumptions about what the binding
of package will be at eval time especially in a multiprocessed lisp,
and I think this is safer.  If you want the string to be read in a
different package,  you can try using :read-package *package*

<read-bindings> is a list of (var . form)'s as in a let statement.
These bindings are made (with the let) before reading the string to
allow for effects such as binding the readtable.

<default-value> (default nil) this establishes a continue restart
which returns this value.  Note that this is slightly different from
protected-eval in that it is always available.

<allow-debug> (default (eq *user-type* :programmer) if true, this
includes a button which allows the debugger to be entered on an error.
Note that the default value is different from protected-eval.

If <local-abort> is true (default nil), then a local restart is
established for abort which returns (values <abort-val> :abort)
where <abort-val> is another parameter. (Same as
protected-eval).

"
  (apply #'prompting-protected-read-from-string string args))


(defun call-prompter (prompt 
		      &rest args
		      &key (stream *query-io*)
			   (local-abort nil)
			   (default-value nil dv?)
			   (abort-val :ABORT)
			   (eval-input? nil)
			   (satisfy-test #'(lambda (obj) T))
		      &allow-other-keys)
  "Prompts user for an input.  <Prompt> is printed with ~A as a prompt.
<stream> defaults to *query-io*.  If <local-abort> is true a local
abort is set up which will return the values <abort-val> and :ABORT.
If <default-value> is supplied, a CONTINUE restart is set up which
allows the user to select the default value.

If <eval-input?> is true, then the expression is evaluated before it
is returned; if not, the unevaluated expression is returned.  

The value supplied by the user is passed to <satisfy-test>.  If that
test fails, the user is prompted again.

This is mostly a dummy function for hiding the prompter type from the
implementatoin mechanism."
  (apply #'prompter prompt stream :allow-other-keys t args))


;;;
;;;----------------------------------------------------------------------
;;; Simple Displays
;;;----------------------------------------------------------------------

(defun displayer (message &key (stream *standard-output*)
			       (beep t)
		  &allow-other-keys)
  "A generic display function which sends a display to the specified
location. <stream> indicates the stream to which the message is to be
sent in the text based version.  <beep> is a logical value indicating
whether or not the device should make some sort of alert signal.  This
is meant to be called through call-displayer."
  (princ  message stream))


(defun call-displayer (message &rest keys
		       &key (stream *standard-output*)
			    (beep t)
		       &allow-other-keys)
    "A generic display function which sends a display to the specified
location. <stream> indicates the stream to which the message is to be
sent in the text based version.  <beep> is a logical value indicating
whether or not the device should make some sort of alert signal.  This
is meant to abstract the means of sending the message from the sending
program."
    (apply #'displayer message :allow-other-keys t keys))




(defun selector (message &key (stream *query-io*)
			      (in-stream stream)
			      (out-stream stream)
			      (option-list '(:yes :no))
		 &allow-other-keys
		 &aux option)
  "This function offers the user a choice of items from a menu of
keywords.  The user can either type the keword or select the option by
number.  <stream> is the stream (default *query-io*) and <option-list>
is the list of options (default '(:yes no)).  <message> is displayed
first on the stream as a prompt."
  (declare (type String message) (type Stream stream in-stream out-stream)
	   (type List option-list)
	   (:returns (type (Member option-list) option)))
  (loop
    (format out-stream  "~&;;;? ~A~%" message)
    (format out-stream ";;;? Select an option by name or number.:~%;;;?(")
    (dotimes (i (length option-list))
      (let ((option (nth i option-list)))
	(format out-stream "~D. ~S    " i option)))
    (format out-stream "::?~%")
    (setq option (read in-stream))
    (if (find option option-list) (return option))
    (if (and (numberp option) (< option (length option-list)))
	(return (nth option option-list)))
    (format out-stream "Unrecognized selection ~S, try again." option)))



(defun call-selector (message &rest keys
		      &key (stream *query-io*)
			   (in-stream stream)
			   (out-stream stream)
			   (option-list '(:yes :no))
		      &allow-other-keys)
  "This function offers the user a choice of items from a menu of
keywords.  The user can either type the keword or select the option by
number.  <stream> is the stream (default *query-io*) and <option-list>
is the list of options (default '(:yes no)).  <message> is displayed
first on the stream as a prompt."
  (declare (type String message) (type Stream stream in-stream out-stream)
	   (type List option-list)
	   (:returns (type (Member option-list) option)))
  (apply #'selector message :allow-other-keys t keys))


(defvar *application-long-name* "Common-Lisp"
  "Name of application for window titles.")

(defvar *application-short-name* "LISP"
  "Name of application for Icon titles.")

