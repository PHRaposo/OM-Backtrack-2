;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   OM-BACKTRACK
;;;
;;;   Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD
;;;
;;; * Based on the original version for OM 4 by Gerard Assayag and Augusto Agon
;;;   Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
;;;   Adapted to OM 7.2 by Paulo Raposo and Karim Haddad
;;;
;;;   OM-BACKTRACK VERSION 2.0 is an expansion of the previous version.
;;;   Copyright (C) 2024 - Paulo Henrique Raposo
;;;
;;;   LISP LIBRARIES:
;;;   
;;; * SCREAMER 4.0.0
;;;   Based on original version 3.20 by:
;;;
;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;
;;;   Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;;   Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;;   Copyright 1993 University of Toronto. All rights reserved.
;;;
;;;   Maintaner: Nikodemus Siivola <https://github.com/nikodemus/screamer>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :om)

;--------------------------------------------------
;Variable definiton with files to load 
;--------------------------------------------------

(defvar *backtrack-files* nil)
(setf  *backtrack-files* (list	
                         (om::om-relative-path '("sources" "screamer 4.0.0") "package")
                         (om::om-relative-path '("sources" "screamer 4.0.0") "screamer")
                         (om::om-relative-path '("sources") "screaminterface") 					 
                         (om::om-relative-path '("sources") "screamer-additions")						 					 						 				 					  
			 (om::om-relative-path '("sources") "screamboxes")
                         (om::om-relative-path '("sources") "screamfuns")		 					 
			 (om::om-relative-path '("sources") "non-deter-patch")								  							 
                          ))
						 
;--------------------------------------------------
;Loading files 
;--------------------------------------------------
(mapc #'compile&load *backtrack-files*)

;--------------------------------------------------
;Fill library 
;--------------------------------------------------


(fill-library '(("primitives" nil nil (either fail) nil)
				("variables" nil nil (an-integer-between a-member-of a-random-member-of list-of-members-of list-of-random-members-of list-of-integers-between a-chord-in 
	                                   list-of-chords-in) nil)
				("constraints" nil nil (apply-cont assert! alldiff? growing?) nil)
				("valuation" nil nil (one-value all-values print-values ith-value n-values possibly? necessarily?) nil)					
               ))
 				
(print (format nil "
OM-BACKTRACK was based on the original version for OM 4
 by Gerard Assayag and Augusto Agon
 Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.
   
 It was adapted to OM 7.2 by Paulo Henrique Raposo and Karim Haddad

* OM-BACKTRACK VERSION 2.0 is an expansion of the previous version.
  Copyright (C) 2024 - Paulo Henrique Raposo
   
  LISP LIBRARIES:
 
* SCREAMER ~A
  Based on original version 3.20 by Jeffrey Mark Siskind and David Allen McAllester
  Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
  Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
  Copyright 1993 University of Toronto. All rights reserved.
    
  Maintaner: Nikodemus Siivola <https://github.com/nikodemus/screamer>" 
s::*screamer-version*))
