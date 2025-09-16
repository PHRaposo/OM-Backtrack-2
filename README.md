# OM-Backtrack 2.0


* OM-BACKTRACK is based on the original version for OM 4
   by Gerard Assayag and Augusto Agon
   Copyright (C) 1997-2003 by IRCAM-Centre Georges Pompidou, Paris, France.


Adapted to OM 7.2 by Paulo Henrique Raposo and Karim Haddad


* OM-BACKTRACK VERSION 2.0 is an expansion of the previous version.


  Copyright (C) 2024 - Paulo Henrique Raposo
 
 
* WARNING: BECAUSE THE MODIFICATIONS IN THE NONDETERMINISTIC PATCHES MECHANISM, THIS VERSION IS NO LONGER COMPATIBLE WITH THE PREVIOUS VERSION.

 
* CHANGES IN VERSION 2.0:
 
 
 -> REMOVED PREFERENCES MODULE AND GLOBAL EVALUATIONS;
 
 
 -> NONDETERMINISTIC PATCHES WAS COMPLETED REVISED AND NOW SUPPORTS LISP FUNCTIONS, LISP PATCHES AND SUB PATCHES;
 
 
 -> ADDED LOCAL EVALUATIONS: ONE-VALUE, ALL-VALUES, PRINT-VALUES, ITH-VALUE, N-VALUES, POSSIBLY? AND NECESSARILY?;


 -> NEW METHODS: EITHER, FAIL, ASSERT!, APPLY-NONDETERMINISTIC AND FUNCALL-NONDETERMINISTIC;

 -> NEW TUTORIAL PATCHES BASED ON ORIGINAL PAPERS BY SCREAMER'S AUTHORS.
 
   
  LISP LIBRARIES:


* SCREAMER 4.0.0
  Based on original version 3.20 by Jeffrey Mark Siskind and David Allen McAllester
  Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
  Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
  Copyright 1993 University of Toronto. All rights reserved.

  Maintaner: Nikodemus Siivola <https://github.com/nikodemus/screamer>

#### OM-Backtrack is an adaptation of the backtrack part of SCREAMER
#### to the paradigm of visual programming language of Openmusic.

#### SCREAMER is an extension of Common Lisp that adds support for nondeterministic
#### programming. Screamer consists of two levels. The basic nondeterministic level
#### adds support for backtracking and undoable side effects. On top of this
#### nondeterministic substrate, Screamer provides a comprehensive constraint
#### programming language in which one can formulate and solve mixed systems of
#### numeric and symbolic constraints. Together, these two levels augment Common
#### Lisp with practically all of the functionality of both Prolog and constraint
#### logic programming languages such as CHiP and CLP(R). Furthermore, Screamer is
#### fully integrated with Common Lisp. Screamer programs can coexist and
#### interoperate with other extensions to as CLIM and Iterate.

#### The version of SCREAMER that is included here have a few modifications, listed below:


#### - Fix for "+-rule-down" and "*-rule-down". [info](https://github.com/nikodemus/screamer/pull/15)


####  - Fix bug and CONS in "apply-nondeterministic-nondeterministic". [info](https://github.com/nikodemus/screamer/pull/28)


####  - Changes in "assert!-notv-equalv" by Swapneil Singh. [info](https://github.com/nikodemus/screamer/pull/34/commits/794719d8a9ee60388f9484b7944a1838a35a059c)


####  - New version of macro "print-values". This version was included in the original OM-Backtrack and
####   it's a counterpart of the original macro for adapted to OM.


####  - New experimental random function: "a-random-member-of".


ORIGINAL OM-BACKTRACK TUTORIAL PAGE (BY CHARLOTTE TRUCHET): . [http://recherche.ircam.fr/equipes/repmus/OpenMusic/user-doc/DocFiles/backtrackTutorial/ ](http://recherche.ircam.fr/equipes/repmus/OpenMusic/user-doc/DocFiles/backtrackTutorial/)


OM-BACKTRACK SCREENSHOT:

![alt text](https://github.com/PHRaposo/OM-Backtrack/blob/ac079e9583c2e95b70e063da8270ba6494738ff5/screenshot.png)

# [Download](https://github.com/PHRaposo/OM-Backtrack/archive/refs/heads/V2.0.zip)


