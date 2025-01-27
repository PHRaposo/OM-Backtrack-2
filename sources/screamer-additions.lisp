(in-package :screamer)
			 	
(defmacro-compile-time print-values (&body forms)
  "Evaluates EXPRESSIONS as an implicit PROGN and outputs
each of the nondeterministic values returned by the last EXPRESSION in
succession using a LISTENER WINDOW (Openmusic).

After each value is printed, the user is queried as to whether or not further
values are desired. These values are produced by repeatedly evaluating the
body and backtracking to produce the next value, until either the user
indicates that no further values are desired or until the body fails and
yields no further values.

Accordingly, local side effects performed by the body while producing each
value are undone after printing each value, before attempting to produce
subsequent values, and all local side effects performed by the body are undone
upon exit from PRINT-VALUES, either because there are no further values or
because the user declines to produce further values.

A PRINT-VALUES expression can appear in both deterministic and
nondeterministic contexts. Irrespective of what context the PRINT-VALUES
expression appears in, the EXPRESSIONS are always in a nondeterministic
context. A PRINT-VALUES expression itself is always deterministic and always
returns NIL.

PRINT-VALUES is analogous to the standard top-level user interface in Prolog."
`(catch 'succeed
   (for-effects
     (let ((value (progn ,@forms)))         
         (unless (om::non-determinise-listener value)
           (throw 'succeed value))))))

;; OLD VERSION FROM OM 4 (WITH GLOBAL VARIABLE -> PREFERENCES PANEL: REMOVED IN THIS VERSION)
   
;(defmacro-compile-time print-values (&body forms)
; `(catch 'succeed
;    (for-effects
;      (let ((value (progn ,@forms)))         
;        (if (= om::*screamer-valuation* 2)
;          (unless (om::non-determinise-listener value)
;            (throw 'succeed value))
;          (progn (throw 'succeed value) (print value)))))))

(defmacro-compile-time n-values (n
	 		    &body forms)				
"FROM T2L-SCREAMER AND SMC(PWGL):
 Copyright (c) 2007, Kilian Sprotte. All rights reserved.
 TODO - DOC	 
"
 (let ((values (gensym "VALUES-"))
       (last-value-cons  (gensym "LAST-VALUE-CONS-"))
       (value (gensym "VALUE-")))
   `(let ((,values '())
          (,last-value-cons nil)
    (number 0))
      (block n-values
  (for-effects
    (let ((,value (progn ,@forms)))
      (global (cond ((null ,values)
 		    (setf ,last-value-cons (list ,value))
 		    (setf ,values ,last-value-cons))
 		   (t (setf (rest ,last-value-cons) (list ,value))
 		      (setf ,last-value-cons (rest ,last-value-cons))))
 	     (incf number))
      (when (>= number ,n) (return-from n-values)))))
      ,values)))
	  
(defun om-random-value (num)
  (if (= num 0) 0
  (if (< num 0)
    (- (random (- num)))
    (random num))))

(defun nth-random (list)
 (nth (om-random-value (length list)) list))
 
(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'a-random-member-of))

(cl:defun a-random-member-of (sequence)
  "Nondeterministically returns an random element of SEQUENCE. The SEQUENCE must be
either a list or a vector."
  (declare (ignore sequence))
  (screamer-error
   "A-RANDOM-MEMBER-OF is a nondeterministic function. As such, it must be called~%~
   only from a nondeterministic context."))
 
(cl:defun a-random-member-of-nondeterministic (continuation sequence)
(let ((sequence (value-of sequence)))
  (cond
    ((listp sequence)
     (unless (null sequence)
       (choice-point-external
        (loop (if (null (rest sequence)) (return))
	     (let ((random-el (nth-random sequence)))
          (choice-point-internal (funcall continuation random-el))
           (setf sequence (value-of (remove random-el sequence :test #'equal :count 1))))))
       (funcall continuation (first sequence))))
    ((vectorp sequence)
     (let ((n (length sequence)))
       (unless (zerop n)
	    (let ((curr-n n)
		       (n (1- n)))
           (choice-point-external
            (dotimes (i n)
			 (decf curr-n) 
			 (let* ((random-el (aref sequence (om-random-value curr-n))))			      
              (choice-point-internal (funcall continuation random-el))
			  (setf sequence (value-of (remove random-el sequence :test #'equal :count 1))))))
           (funcall continuation (aref sequence 0))))))
    (t (error "SEQUENCE must be a sequence")))))

; ================================================================================================ ;
;; CHANGES FROM SWAPNEILS : https://github.com/swapneils/screamer/tree/master
; ================================================================================================ ;
		
(cl:defun apply-nondeterministic-nondeterministic
    (continuation function argument &rest arguments)
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
	    (apply #'apply (nondeterministic-function-function function)
	               continuation argument arguments)
	        (funcall continuation (apply #'apply function argument arguments)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declare-nondeterministic 'mapcar-nondeterministic))

(cl:defun mapcar-nondeterministic (function &rest arguments)
  "Analogous to the CL:mapcar, except FUNCTION can be either a nondeterministic
function, or an ordinary deterministic function.
You must use mapcar-NONDETERMINISTIC to mapcar a nondeterministic function. An
error is signalled if a nondeterministic function object is used with
CL:mapcar.
You can use MAPCAR-NONDETERMINISTIC to mapcar either a deterministic or
nondeterministic function, though even if all of the ARGUMENTS are
deterministic and FUNCTION is a deterministic function object, the call
expression will still be nondeterministic \(with presumably a single value),
since it is impossible to determine at compile time that a given call to
MAPCAR-NONDETERMINISTIC will be passed only deterministic function objects for
function."
  (declare (ignore function arguments))
  (screamer-error
   "mapcar-NONDETERMINISTIC is a nondeterministic function. As such, it must~%~
   be called only from a nondeterministic context."))

(cl:defun mapcar-nondeterministic-nondeterministic
    (continuation function argument &rest arguments)
  (let ((function (value-of function)))
    (if (nondeterministic-function? function)
        (funcall continuation
                 (apply #'mapcar
                        (lambda (&rest args)
                          (let (res) ;<==fix (phraposo)
                           (apply (nondeterministic-function-function function)
                                  (lambda (r) (push r res)) ;<== push (phraposo)
                                 args)
                            (reverse res))) ;<== reverse (phraposo)
                        argument arguments))
        (funcall continuation (apply #'mapcar function argument arguments)))))

(defun assert!-notv-equalv (x y)
 (cond
   ((known?-equalv x y) (fail))
   ((not (known?-notv-equalv x y))
    (let* ((x (variablize x))
           (y (variablize y))
           (noticer #'(lambda ()
                        (cond ((and (known?-numberpv x)
                                    (known?-numberpv y))
                               (/=-rule x y))
                              ((known?-equalv x y) (fail))))))
      (attach-noticer! noticer x)
      (attach-noticer! noticer y)))))

