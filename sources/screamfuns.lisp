;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD
;;;
;;; NEW FUNCTINS AND METHODS OF VERSION 2.0:
;;;
;;; - A-RANDOM-MEMBER-OF / LIST-OF-RANDOM-MEMBERS-OF
;;;  
;;; - EITHER / FAIL / ALL-VALUES / ONE-VALUES / PRINT-VALUES
;;; ITH-VALUE / N-VALUES / POSSIBLY? / NECESSARILY?
;;; 
;;; - IN-PROGRESS: FOR-EFFECTS / LOCAL / GLOBAL
;;;

(in-package :s)

;(setf *compte-bt* 0)

;(defun fail ()
;  (setf *compte-bt* (1+ *compte-bt*))
;  (throw 'fail nil))

(defun l-and (l)
  (cond ((null l) t)
        ((equal (car l) nil) nil)
        (t (l-and (cdr l)))))

(defun list-of-members-of (n dom)
  (if (zerop n) nil
      (cons (a-member-of dom)
            (list-of-members-of (1- n) dom))))

(defun list-of-random-members-of (n dom)
  (if (zerop n) nil
      (cons (a-random-member-of dom)
            (list-of-random-members-of (1- n) dom))))
						
(defun list-of-integers-between (n low high)
  (if (zerop n) nil
      (cons (an-integer-between low high)
            (list-of-integers-between (1- n) low high))))

(defun cont-chord (l)
  (cond ((null l) nil)
        ((null (cdr l)) l)
        ((> (car l) (cadr l)) (fail))
        ((member (car l) (cdr l)) (fail))
        (t l)))

(defun a-chord-in (n dom &optional prov)
  (if (zerop n) prov
      (a-chord-in (1- n) dom
                  (cont-chord 
                   (cons (a-member-of dom)
                         prov)))))

(defun and-aux (conts var)
  (cond ((null conts) t)
        ((not (apply (car conts) (list var))) nil)
        (t (and (apply (car conts) (list var)) 
                (and-aux (cdr conts) var)))))

(defun list-of-chords-in (l dom &optional cont)
  (cond ((null cont) (list-of-chords-in-simple l dom))
        ((functionp cont) (list-of-chords-in-cont l dom cont))
        ((listp cont) (list-of-chords-in-lcont l dom cont))
        (t (print `(La contrainte doit etre function, nil ou list)))))

(defun list-of-chords-in-simple (l dom)
  (if (null l) nil
      (cons (a-chord-in (car l) dom) 
            (list-of-chords-in (cdr l) dom))))

(defun list-of-chords-in-cont (l dom cont)
  (if (null l) nil
      (cons (apply 
             (lambda (var)
               (if (apply cont (list var))
                 var
                 (s::fail)))
             (list (a-chord-in (car l) dom)))
            (list-of-chords-in-cont (cdr l) dom cont))))

(defun list-of-chords-in-lcont (l dom conts)
  (if (null l) nil
      (cons (apply 
             (lambda (var)
               (if (and-aux conts var)
                 var
                 (s::fail)))
             (list (a-chord-in (car l) dom)))
            (list-of-chords-in-lcont (cdr l) dom conts))))

; DISABLED FOR NOW
#|
(defun assert!-rec (x);same as assert!-all from OM? 
(if (null x) nil
	(if (atom x) (assert! x)
		(if (and (listp x) (every #'atom x))
		    (progn (assert! (car x))
		           (assert!-rec (cdr x)))
			(if (and (listp x) (every #'listp x))
			    (progn (assert!-rec (car (car x)))
				       (assert!-rec (cdr x))))))))
|#		
			   
(in-package :om)

(defmethod get-real-funname ((self t)) self)

(defmethod get-boxcallclass-fun ((self (eql 'an-integer-between))) 'screamerboxes)
(defmethod! an-integer-between  ((low integer) (high integer))
   :initvals '(0 10) 
   :indoc '("low value" "high value")
   :doc "Defines a Screamer variable, in the interval [low high]. 
Without constraints, an-integer-between enumerates all the integers between low and high.

Inputs :
low : integer, minimum of the possible values for the variable
high : integer, maximum of the possible values for the variable

Output:
an integer between low and high. The value depends on the backtracking caused by the constraints
"
   :icon 486 
   (s:an-integer-between low high))

(defmethod get-boxcallclass-fun ((self (eql 'a-member-of))) 'screamerboxes)
(defmethod! a-member-of  ((lst list))
   :initvals '((0 1 2 3 4 5)) 
   :indoc '("list of possible values")
   :doc "Defines a Screamer variable, in the list of values.
Without constraints, an-member-of enumerates all the values of the list.

Inputs :
list : list of possible values

Output:
a value of the list. The value depends on the backtracking caused by the constraints
"
   :icon 486
   (s:a-member-of lst))

(defmethod get-boxcallclass-fun ((self (eql 'a-random-member-of))) 'screamerboxes)
(defmethod! a-random-member-of  ((lst list))
 :initvals '((0 1 2 3 4 5)) 
 :indoc '("list of possible values")
 :doc "Defines a Screamer variable, in the list of values.
Without constraints, an-member-of enumerates all the values of the list in random order.

Inputs :
list : list of possible values

Output:
a value of the list. The value depends on the backtracking caused by the constraints
"
 :icon 486
 (s::a-random-member-of lst))
 
(defun appc (fun variables)
  (apply 
   (lambda (var)
     (if (apply fun (list var))
       var
       (s::fail)))
   (list variables))
  variables)

(defmethod!  apply-cont ((fun function) (var om::t))  
  :indoc '("Constraint in lambda-mode" "Variables")
  :initvals '(nil nil) 
  :icon 486
  :doc "Applies the constraint (patch in lambda-mode) to the variables

Inputs :

fun : an anonymous function, with outputs t or nil (ie, a predicate)
      or a list of anonymous functions
var : variables defined with Screamer primitives, like an-integer-between, a-member-of, etc.
The type of the predicate's inputs must be the same as the variables'.

Output:

If the predicate is true (if they are all true in case of a list) on the variables, the variables. 
Else, apply-cont causes backtrack.
"
  (appc fun var))

(defmethod!  apply-cont ((funs list) (var om::t))  
  :indoc '("List of constraints in lambda-mode" "Variables")
  :initvals '(nil nil)  
  :icon 486
  (loop for fun in funs
        do
        (appc fun var))
  var)

(defmethod get-boxcallclass-fun ((self (eql 'list-of-members-of))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-members-of))) self)
(defmethod! list-of-members-of  ((n integer) (dom list))
  :initvals '(2 '(1 2 3))
  :indoc '("number of variables" "list of values")
  :doc "Defines a list of Screamer variables.
Each variable is a member of dom. 

Inputs :
n : length of the list
dom : domain for each variable

Output : a list of n variables in dom. 
The value depends on the backtracking caused by the constraints
" 
  :icon 486 
  (s::list-of-members-of n dom))

(defmethod get-boxcallclass-fun ((self (eql 'list-of-random-members-of))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-random-members-of))) self)
(defmethod! list-of-random-members-of  ((n integer) (dom list))
:initvals '(2 '(1 2 3))
:indoc '("number of variables" "list of values")
:doc "Defines a list of Screamer variables.
Each variable is a member of dom. 

Inputs :
n : length of the list
dom : domain for each variable

Output : a list of n variables in dom. 
The value depends on the backtracking caused by the constraints
" 
:icon 486 
(s::list-of-random-members-of n dom))

(defmethod get-boxcallclass-fun ((self (eql 'list-of-integers-between))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-integers-between))) self)
(defmethod! list-of-integers-between  ((n integer) (low integer) (high integer))
  :initvals '(2 0 10)
  :indoc '("number of variables" "low" "high")
  :doc "Defines a list of Screamer variables. 
Each variable is an integer between low and high. 

Inputs :
n : length of the list
low : integer, minimum of the possible values for each variable
high : integer, maximum of the possible values for each variable

Output : a list of n integer variables between low and high. 
The value depends on the backtracking caused by the constraints" 
  :icon 486 
  (s::list-of-integers-between n low high))

(defmethod get-boxcallclass-fun ((self (eql 'a-chord-in))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'a-chord-in))) self)
(defmethod! a-chord-in  ((n integer) (dom list))
   :initvals '(5 '(6000 6200 6400 6600 6800 7000))
   :indoc '("number of variables" "list of values")
   :doc "Defines a list of Screamer variables.
Each variable is a member of dom. a-chord-in contains two predefined constraints, they state that the values in the list are all different (you can't get (1 1 1)), and the list is sorted (you can't get (2 1 3)). 
So the output looks more like a set, which is usefull for chords definition.

Inputs :
n : length of the list
dom : domain for each variable

Output : a list of n sorted and all different variables in dom. 
The value depends on the backtracking caused by the constraints
" 
   :icon 486 
   (s::a-chord-in n dom))

(defmethod get-boxcallclass-fun ((self (eql 'list-of-chords-in))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'list-of-chords-in))) self)
(defmethod! list-of-chords-in  ((l list) (dom list) &optional prov)
  :initvals '((4 3 2) '(6000 6200 6400 6600 6800 7000 7200))
  :indoc '("number of variables" "domains" "constraints on variables")
  :doc "Defines a list of a-chord-in, ie a list of all-different and sorted lists of Screamer variables. 
Each variable is a member of dom. 

The optional input allows to state constraints on the intermediate lists. 
For instance, when list-of-chords-in is used to define a chordseq,
the constraints state on the chords.

Inputs :
l : length of the intermediate lists. For instance, (1 2 3) will give something like ((0) (0 0) (0 0 0)).
dom : domain, ie possible values for the variables
cont : an anonymous function, outputs nil or t (ie a predicate)
       or a list of such anonymous functions
       The type of the cont's input must be the same as the variables'.

Output : a list of lists of all different, sorted variables in dom. 
The value depends on the backtracking caused by the constraints" 
  :icon 486 
  (s::list-of-chords-in l dom prov))

(defun alldiff2 (l)
  (cond ((null l) t)
        ((member (car l) (cdr l) :test #'equal) nil)
        (t (alldiff2 (cdr l)))))
 
(defmethod! alldiff? ((l list))
  :initvals '(nil)
  :indoc '("a list")
  :doc "True iff all the values of the list are different
Input : a list

Output : nil if the list contains the same value twice
         t if all the values in the list are differents.
" 
  :icon 486 
  (alldiff2 l))

(defun croissante (l)
  (cond ((null (cdr l)) t)
        ((>= (car l) (cadr l)) nil)
        (t (croissante (cdr l)))))

(defmethod! growing? ((l list))
  :initvals '(nil)
  :indoc '("a list")
  :doc "True iff all the list is growing
Input : a list

Output : t if the list is growing
         nil otherwise
" 
  :icon 486 
  (croissante l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NEW METHODS ===> OM-BACKTRACK 2.0 - PHRAPOSO

(defmethod get-boxcallclass-fun ((self (eql 'either))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'either))) self)
(defmethod! either  (&rest expressions)
  :initvals '(nil)
  :indoc '("expressions")
  :doc "OM equivalent of SCREAMER::EITHER." 
  :icon 486 
  (screamer::either (apply #'sequence expressions)))

(defmethod! fail ()
  :initvals '()
  :indoc '()
  :doc "OM equivalent of SCREAMER::FAIL." 
  :icon 486 
  (screamer::fail))

(defmethod get-boxcallclass-fun ((self (eql 'apply-nondeterministic))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'apply-nondeterministic))) self)
(defmethod! apply-nondeterministic  ((fun function) &rest arguments)
  :initvals '(nil nil)
  :indoc '("lambda function" "variables")
  :doc "OM equivalent of SCREAMER::APPLY-NONDETERMINISTIC." 
  :icon 486 
  (screamer::apply-nondeterministic fun arguments))

(defmethod get-boxcallclass-fun ((self (eql 'funcall-nondeterministic ))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'funcall-nondeterministic ))) self)
(defmethod! funcall-nondeterministic  ((fun function) &rest arguments)
  :initvals '(nil nil)
  :indoc '("lambda function" "variables")
  :doc "OM equivalent of SCREAMER::FUNCALL-NONDETERMINISTIC." 
  :icon 486 
  (screamer::funcall-nondeterministic fun arguments))

(defmethod get-boxcallclass-fun ((self (eql 'mapcar-nondeterministic ))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'mapcar-nondeterministic ))) self)
(defmethod! mapcar-nondeterministic  ((fun function) &rest arguments)
  :initvals '(nil nil)
  :indoc '("lambda function" "variables")
  :doc "OM equivalent of SCREAMER::MAPCAR-NONDETERMINISTIC." 
  :icon 486 
  (screamer::mapcar-nondeterministic fun arguments))
  
;; TODO: FOR-EFFECTS / LOCAL / GLOBAL
(defmethod get-boxcallclass-fun ((self (eql 'for-effects))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'for-effects))) self)
(defmethod! for-effects (&rest forms)
:initvals '(nil)
:indoc '("forms" "environment")
:doc "OM equivalent of SCREAMER::FOR-EFFECTS macro." 
:icon 486 
(screamer::for-effects forms))

(defmethod get-boxcallclass-fun ((self (eql 'local))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'local))) self)
(defmethod! local (&rest body)
:initvals '(nil)
:indoc '("body")
:doc "OM equivalent of SCREAMER::LOCAL macro." 
:icon 486 
(screamer::local body))

(defmethod get-boxcallclass-fun ((self (eql 'global))) 'screamerboxes)
(defmethod get-real-funname ((self (eql 'global))) self)
(defmethod! global (&rest body)
:initvals '(nil)
:indoc '("body")
:doc "OM equivalent of SCREAMER::GLOBAL macro." 
:icon 486 
(screamer::global body))

(defmethod! assert! ((x t))
:initvals '(nil)
:indoc '("boolean")
:doc "OM equivalent of SCREAMER::ASSERT! macro." 
:icon 486 
(screamer::assert! x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCREAMER VALUATION METHODS
	  
(defmethod get-boxcallclass-fun ((self (eql 'one-value))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'one-value))) self)
(defmethod! one-value ((expression t) &optional (default-expression '(screamer::fail)))
:initvals '(nil '(screamer::fail))
:indoc '("expressions" "fail")
:doc "OM equivalent of SCREAMER::ONE-VALUE macro." 
:icon 486 
(screamer::one-value expression default-expression))	
	    
(defmethod get-boxcallclass-fun ((self (eql 'all-values))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'all-values))) self)
(defmethod! all-values  ((expressions t))
:initvals '(nil)
:indoc '("expressions")
:doc "OM equivalent of SCREAMER::ALL-VALUES macro." 
:icon 486 
(screamer::all-values expressions))	

(defmethod get-boxcallclass-fun ((self (eql 'print-values))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'print-values))) self)
(defmethod! print-values  ((forms t))
:initvals '(nil)
:indoc '("forms")
:doc "OM verion of SCREAMER:PRINT-VALUES macro. It opens a listener window." 
:icon 486 
(screamer::print-values forms))

(defmethod get-boxcallclass-fun ((self (eql 'ith-value))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'ith-value))) self)
(defmethod! ith-value  ((i integer) (forms t) &optional (default-expression '(screamer::fail)))
:initvals '(10 nil '(screamer::fail))
:indoc '("integer" "forms" "fail")
:doc "OM verion of SCREAMER:ITH-VALUE macro." 
:icon 486 
(screamer::ith-value i forms default-expression))	

(defmethod get-boxcallclass-fun ((self (eql 'n-values))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'n-values))) self)
(defmethod! n-values ((n integer) (forms t))
:initvals '(10 nil)
:indoc '("integer" "forms")
:doc "OM version of SCREAMER:N-VALUES macro.
FROM T2L-SCREAMER AND SMC(PWGL):
Copyright (c) 2007, Kilian Sprotte. All rights reserved." 
:icon 486 
(screamer::n-values n forms))

(defmethod get-boxcallclass-fun ((self (eql 'possibly?))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'possibly?))) self)
(defmethod! possibly?  ((expressions t))
:initvals '(nil)
:indoc '("expressions")
:doc "OM equivalent of SCREAMER::POSSIBLY? macro." 
:icon 486 
(screamer::possibly? expressions))

(defmethod get-boxcallclass-fun ((self (eql 'necessarily?))) 'screamer-valuation-boxes)
(defmethod get-real-funname ((self (eql 'necessarily?))) self)
(defmethod! necessarily?  ((expressions t))
:initvals '(nil)
:indoc '("expressions")
:doc "OM equivalent of SCREAMER::NECESSARILY? macro." 
:icon 486 
(screamer::necessarily? expressions))

