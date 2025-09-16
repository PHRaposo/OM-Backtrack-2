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

