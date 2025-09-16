-*- mode: text; mode: auto-fill; fill-column: 78 -*-

Screamer is an extension of Common Lisp that adds support for nondeterministic
programming. Screamer consists of two levels. The basic nondeterministic level
adds support for backtracking and undoable side effects. On top of this
nondeterministic substrate, Screamer provides a comprehensive constraint
programming language in which one can formulate and solve mixed systems of
numeric and symbolic constraints. Together, these two levels augment Common
Lisp with practically all of the functionality of both Prolog and constraint
logic programming languages such as CHiP and CLP(R). Furthermore, Screamer is
fully integrated with Common Lisp. Screamer programs can coexist and
interoperate with other extensions to as CLIM and Iterate.

In several ways Screamer is more efficient than other implementations of
backtracking languages. First, Screamer code is transformed into Common Lisp
which can be compiled by the underlying Common Lisp system. Many competing
implementations of nondeterministic Lisp are interpreters and thus are far
less efficient than Screamer. Second, the backtracking primitives require
fairly low overhead in Screamer. Finally, this overhead to support
backtracking is only paid for by those portions of the program which use the
backtracking primitives. Deterministic portions of user programs pass through
the Screamer-to-Common-Lisp transformation unchanged. Since in practise, only
small portions of typical programs utilize the backtracking primitives,
Screamer can produce more efficient code than compilers for languages in which
backtracking is more pervasive.

Screamer was written by Jeffrey Mark Siskind and David Allen McAllester, see
file LICENSE for licensing information.

This version of Screamer is based on released version 3.20, and is
being dragged kicking -- and screaming -- into the 21st century:

* Support for Symbolics, AKCL, etc. has been stripped.

* It has been modified to run in ANSI Common Lisp, as opposed to CLtL1/CLtL2.

* Ongoing maintenance work: original screamer.lisp has been split into
  package.lisp and screamer.lisp, ChangeLog.old, and some information
  has been moved over to TODO.

* Ongoing development and documentation work: a new manual is in the making,
  and add support for missing Common Lisp special forms in nondeterministic
  context is planned. ...the progress is rather glacial, however. Don't hold
  your breath.

See TODO for the current task list.

Source files part of the distribution not currently referenced in the
screamer.asd:

screams.lisp - A file containing all of the examples from the Screamer manual
and the two papers ircs-93-03 and aaai93. To use, first compile and load
Screamer and Iterate, compile and load this file, and then type
(IN-PACKAGE :SCREAMS).

equations.lisp - A file containing some equations for testing Screamer's
numeric constraint satisfaction procedures. To use, first compile and load
Screamer, compile and load this file, and then type (IN-PACKAGE :SCREAMS).

iscream.el - If you run Lisp on Unix under GNUEmacs using ILisp you can load
this Emacs Lisp file (preferably byte compiled first). You must also then
set the variable SCREAMER:*ISCREAM?* to T. This will enable the Screamer
macro LOCAL-OUTPUT and improve the behavior of Y-OR-N-P and PRINT-VALUES
under ILisp.

Subdirectory papers/ contains the original Screamer manual and papers:

screamer.pdf
screamer.dvi
screamer.ps - PDF, DVI, and Postscript versions of an outdated manual for
Screamer. The code in this manual has some bugs but corrected versions are
included in screams.lisp.

ircs-93-03.pdf
ircs-93-03.dvi
ircs-93-03.ps - PDF, DVI, and Postscript versions of a paper describing the
fundamentals of nondeterministic CommonLisp. This paper is available at
Technical Report 93-03 of the University of Pennsylvania Institute for
Research in Cognitive Science. The appropriate BibTeX entry is:
\newcommand{\Screamer}{{\mbox{\sc Screamer}}}
\newcommand{\CommonLisp}{{\mbox{\sc Common Lisp}}}
@string{IRCS = {University of Pennsylvania Institute for Research in Cognitive
	Science}}
@techreport{SiskindM93,
	author = {Jeffrey Mark Siskind and David Allen McAllester},
	title = {{\Screamer:} A Portable Efficient Implementation of
		Nondeterministic {\CommonLisp}},
	institution = IRCS,
	year = 1993,
        number = {IRCS--93--03}}
The code in this paper is included in screams.lisp.

aaai93.pdf
aaai93.dvi
aaai93.ps - PDF, DVI, and Postscript versions of a paper describing the
constraint package included with Screamer. This paper will appear in the
Proceedings of AAAI-93. The appropriate BibTeX entry is:
The code in this paper is also included in screams.lisp.
\newcommand{\Lisp}{{\mbox{\sc Lisp}}}
@string{AAAI93 = {Proceedings of the Eleventh National Conference on
	Artifical Intelligence}}
@inproceedings{SiskindM93a,
	author = {Jeffrey Mark Siskind and David Allen McAllester},
	title = {Nondeterministic {\Lisp} as a Substrate for Constraint Logic
		Programming},
	booktitle = AAAI93,
	year = 1993,
	month = jul}

Following are old notes regarding incompatibilities between Screamer 3.20 and
Screamer 2.4 (as eg. described in papers/screamer.ps):

Screamer 3.20 contains numerous bug fixes, performance enhancements and novel
features over Screamer 2.4, the prior widely released version. I do not have
the time to describe all such improvements. Until the completion of a new
Screamer manual you must resort to looking at the source code. At the beginning
of the file there is a fairly extensive change log.

A small number of incompatibilities have been introduced in the transition
from Screamer 2.4 to Screamer 3.20. These are summarized below. Those already
familiar with Screamer should have no difficulty modifying their code modulo
these changes.

1. All Screamer code must be recompiled. The Screamer 3.20 runtime is
incompatibile with the Screamer 2.4 compiler.

2. The function MAP-VALUES has been removed. An expression such as:
(MAP-VALUES function expression)
can be rewritten using the new FOR-EFFECTS macro as follows:
(FOR-EFFECTS (FUNCALL function expression))
The new syntax is every bit as powerful as the old syntax. In fact it is more
powerfull. MAP-VALUES used to require that the function argument be a
deterministic expression while the new syntax makes no such requirement. (Note
that FUNCALL still requires that its first argument evaluate to a deterministic
function.)

3. You no longer need to reload Screamer after doing an UNWEDGE-SCREAMER since
Screamer keeps track of which functions are intrinsic and UNWEDGE-SCREAMER
does not purge those functions.

4. The following functions have been renamed:
NUMBERV  -> NUMBERPV
REALV    -> REALPV
INTEGERV -> INTEGERPV
BOOLEANV -> BOOLEANPV
The original names were inconsistent with the naming convention that every
function ending in V names a lifted version of the function name without the V.
I.e. NUMBERV would have been a lifted version of a function NUMBER but there
is no ground function. NUMBERV was really a lifted version of NUMBERP and thus
should have been named NUMBERPV.

5. A new naming convention has been introduced. All nondeterministic
`generators' now begin with the prefix A- or AN-. This results in the
following name changes:
INTEGER-BETWEEN -> AN-INTEGER-BETWEEN
MEMBER-OF       -> A-MEMBER-OF
FLIP            -> A-BOOLEAN
Furthermore, `lifted generators' both begin with A- or AN- and end with V.
This results in the following name changes:
REAL-ABOVEV      -> A-REAL-ABOVEV
REAL-BELOWV      -> A-REAL-BELOWV
REAL-BETWEENV    -> A-REAL-BETWEENV
INTEGER-ABOVEV   -> AN-INTEGER-ABOVEV
INTEGER-BELOWV   -> AN-INTEGER-BELOWV
INTEGER-BETWEENV -> AN-INTEGER-BETWEENV

6. The variable *FUZZ* has been eliminated. The functionality of this variable
has been replaced by additional arguments to the REORDER function.

7. REORDER now takes four arguments:
(COST-FUNCTION TERMINATE? ORDER FORCE-FUNCTION)
instead of one. The FORCE-FUNCTION is the same as the prior lone argument.
The COST-FUNCTION is a function to be applied to each VARIABLE at each
reordering step to return its cost. Typical values for COST-FUNCTION are
#'DOMAIN-SIZE or #'RANGE-SIZE. The COST-FUNCTION can return NIL which causes
REORDER to not consider that variable for further forcing. ORDER is a two
argument predicate applied to the non-NIL cost functions computed for the
variables at each reordering step. Typical values are #'<, to choose the least
cost, and #'>, to choose the greatest cost variable to force next. TERMINATE?
is a one argument predicate applied to the (non-NIL) cost function computed
for the variable chosen to force next. If TERMINATE? returns T then the
variable reordering and forcing terminates. The following is a typical call
to REORDER used to solve numerical constraints:

(REORDER #'RANGE-SIZE
         #'(LAMBDA (X) (< X 1E-6))
         #'>
         #'DIVIDE-AND-CONQUER-FORCE)

The following is a typical call to REORDER used to solve symbolic constraints:

(REORDER #'DOMAIN-SIZE
         #'(LAMBDA (X) (DECLARE (IGNORE X)) NIL)
         #'<
         #'LINEAR-FORCE)

8. Instead of the standard Screamer file preamble which used to be:

(IN-PACKAGE :<my-package>)
(USE-PACKAGE '(:LISP :SCREAMER))
(SHADOWING-IMPORT '(SCREAMER::DEFUN))

there is now a different standard preamble. Loading Screamer creates a
predefined package SCREAMER-USER which is useful for small student and
demonstration programs. If you wish your file to be in the SCREAMER-USER
package the single line:

(IN-PACKAGE :SCREAMER-USER)

should be placed at the top of the file. In addition:

(IN-PACKAGE :SCREAMER-USER)

should be typed to the Listener after loading Screamer. More complex programs
typically reside in their own package. You can place a program in its own
package by using the following preamble to your file:

(IN-PACKAGE :CL-USER)
(SCREAMER:DEFINE-SCREAMER-PACKAGE :<my-package>
 <optional defpackage arguments>)
(IN-PACKAGE :MY-PACKAGE)

9. Version 4.0.1 (by Paulo Raposo) - Rational numbers are now supported in Screamer.
The following functions have been added:

A-RANDOM-MEMBER-OF
A-RATIONAL
A-RATIONAL-ABOVE
A-RATIONAL-BELOW
A-RATIONAL-BETWEEN
A-RATIO
A-RATIO-ABOVE
A-RATIO-BELOW
A-RATIO-BETWEEN
A-RATIONALPV
A-RATIONAL-BETWEENV
A-RATIONAL-ABOVEV
A-RATIONAL-BELOWV
A-RATIOPV
A-RATIO-BETWEENV
A-RATIO-ABOVEV
A-RATIO-BELOWV
FLOATPV
A-FLOATV
A-FLOAT-ABOVEV
A-FLOAT-BELOWV
A-FLOAT-BETWEENV
A-RANDOM-MEMBER-OFV
N-VARIABLES
N-LISTS-OF VARIABLES
RANDOM-FORCE

The following functions have been added/modified to support rational numbers:
LINEAR-FORCE
DIVIDE-AND-CONQUER-FORCE
FINITE-DOMAIN?
=-RULE (use enumerated-domains)
/=-RULE (use enumerated-domains)
DOMAIN-SIZE

The following functions have been added to support Farey sequences (unexported):
RATIOP
FAREY-NEXT-VALUE
FAREY-PREV-VALUE
MAKE-FAREY-GENERATOR-START
SAFEST-FAREY-RANGE-SIZE
ESTIMATE-FAREY-DOMAIN-SIZE
CLOSEST-RATIONAL-LOWER
CLOSEST-RATIONAL-UPPER
CLOSEST-RATIO-LOWER
CLOSEST-RATIO-UPPER
RATIONALS-BETWEEN
RATIOS-BETWEEN
VARIABLE POSSIBLY-NONINTEGER-RATIONAL? SLOT
VARIABLE MAX-DENOM SLOT

Additions and fixes from swapneils update (see https://github.com/nikodemus/screamer/pull/34)
APPLY-NONDETERMINISTIC FIX
MAPCAR-NONDETERMINISTIC
ASSERT!-NOTV-EQUALV FIX
VARIABLE DEPENDENCIES SLOT
LAMBDA-NONDETERMINISTIC
APPLY-SUBSTITUTION
OCCURS-IN?
BOUNDED? - (modified to include rationals)


Constraint package functions modified to use the new variable generators (see initial commit):
 - ALL VARIABLES PREDICATES (VARIABLE-INTEGER?, VARIABLE-NONINTEGER?, etc.)
 - ALL RESTRICTION FUNCTIONS (RESTRICT-INTEGER!, RESTRICT-NONINTEGER!, etc.)
 - ALL VARIABLE LOWER/UPPER BOUNDS AND DOMAINS PRUNING (RESTRICT-BOUNDS!, RESTRICT-ENUMERATED-DOMAIN!, etc.)
 - ALL ARITHMETIC FUNCTIONS AND RULES (+V, +-RULE-UP, +-RULE-DOWN, etc.)
 - NEW RULES FOR MAXIMUM-RATIO-DENOMINATOR
 - KNOWN?- AND ASSERT!- FUNCTIONS (KNOWN?-RATIOPV, ASSERT!-RARTIOPV, etc.)
 - TRANSFORM-KNOWN?, TRANSFORM-ASSERT!- AND TRANSFORM-DECIDE

New macros:
OPTIMIZE-VALUE (based on BEST-VALUE, see best-optimize-value-examples.lisp)
N-VALUES (by Killian Sprotte, from PWGL Community Library Repository: https://github.com/JulienVincenot/PWGL-community-library/blob/main/User-library/smc-0.2.7/screamer-additions.lisp)

New global variables: 
*maximum-list-domain-size*
*maximum-random-domain-size*

New tests files and tests.lisp modifications
 - rational-tests.lisp
 - variable-types.lisp
 - funcallv-tests.lisp
 - tests.lisp
   - runs screamer::rational-ordeal, screamer::variable-type-tests and screamer::test-funcallv under Stefil
   - TODO: APPLYV tests
   
Additional changes:
 - Primordial.lisp (Lispworks and Allegro Common LISP compatibility)  
 - Clozure Common LISP support (SCREAMER::WALK)
 - Added Alexandria as a dependency in ASDF system definition.

 
 Examples (third argument of rational/ratio generators are the maximum ratio denominator): 
> (all-values (a-rational-between 0 1 4))
(0 1/4 1/3 1/2 2/3 3/4 1)

> (all-values (a-ratio-between 0 1 4))
(1/4 1/3 1/2 2/3 3/4)

> (let ((x (a-rational-betweenv 0 1 4))) (all-values (solution x (static-ordering #'linear-force))))
(0 1/4 1/3 1/2 2/3 3/4 1)

> (let ((x (a-ratio-betweenv 0 1 4))) (all-values (solution x (static-ordering #'linear-force))))
(1/4 1/3 1/2 2/3 3/4)

SEE ADDITIONAL EXAMPLES IN RATIONAL-TESTS.LISP

Enhancements: 
  - FUNCALLV and APPLYV
  
 This update introduces a new version of the APPLYV and FUNCALLV functions
 in Screamer, extending their capabilities while (hopefully) maintaining
 full backward compatibility with existing codebases.

 Backward Compatibility
 The updated functions preserve the original behavior of APPLYV and FUNCALLV,
 ensuring seamless integration with legacy Screamer code.

 Extended Constraint Application
 The new implementation expands the scope of constraint propagation beyond
 atomic variables. It now supports:
   - Lists of variables
   - Trees (lists of lists)
   - TODO: other types (vectors, arrays, hash-tables, etc.) 
 This allows constraints to be applied recursively across structured data,
 enabling more expressive and flexible constraint modeling.

 Domain Propagation Strategies
 To support efficient constraint satisfaction, the enhanced functions integrate
 Screamer's classical domain propagation strategies:
   - :GFC - Generalized Forward Checking
   - :AC  - Arc Consistency

 These strategies are automatically applied to propagate domain restrictions
 across complex variable structures.

 Automatic Domain Generation
 A new feature enables automatic domain generation when a constraint is applied,
 provided the domain size remains within a configurable limit. This is controlled
 by the global variable:

   *maximum-list-domain-size* Default: 100

 - If the estimated domain size exceeds this threshold, automatic generation is skipped.
 - Users can override the default value to accommodate larger domains as needed.

Modified functions:
KNOWN?-CONSTRAINT
PROPAGATE-GFC
PROPAGATE-AC
ASSERT!-CONSTRAINT-GFC
ASSERT!-CONSTRAINT-AC
ASSERT!-CONSTRAINT
KNOWN?-FUNCALLV
KNOWN?-NOTV-FUNCALLV
ASSERT!-FUNCALLV
ASSERT!-NOTV-FUNCALLV
FUNCALLV
ARGUMENTS-FOR-APPLYV
KNOWN?-APPLYV
KNOWN?-NOTV-APPLYV
ASSERT!-APPLYV
ASSERT!-NOTV-APPLYV
APPLYV

New functions:
DEEP-VALUE-OF
DEEP-BOUND?
DEEP-GROUND?
DEEP-FINITE-DOMAIN?
FLATTEN
SUBSTITUTE-VARIABLE
PROPAGATE-DOMAIN
SET-CONSTRAINT-ENUMERATED-DOMAIN!

SEE EXAMPLES IN FUNCALLV-TESTS.LISP


New feature: SCREAMER-EXTENSIBLE-TYPES (EXPERIMENTAL)

This update introduces a new feature in Screamer that allows users to define
custom variable types, enhancing the flexibility and extensibility of the
constraint system. Users can now create and register their own types, which
can be used in constraints, variable declarations, and other parts of the
Screamer framework. The possible types are now tracked from enumerated domains
of non number variables.

About canonical-type and Type Order

The function CANONICAL-TYPE is central to Screamer's extensible type system.
It determines the canonical type of an object for use in constraints and
variable declarations. The order of type checks in CANONICAL-TYPE is important:
for example, proper lists are classified as 'list rather than 'cons, because the
check for list comes before cons. This ensures that each object is assigned the
most specific and appropriate type.

When extending Screamer with new types, make sure to update CANONICAL-TYPE and
carefully consider the order in which types are checked. This guarantees correct
classification and avoids unintended overlaps (e.g., distinguishing between lists
and cons cells).

Defining Custom Types

After enabling this feature by compiling Screamer with the #+screamer-extensible-types
feature, users can define a new type in two ways:

1. Global Variable Method (Recommended for system-wide types):
Add the type symbol to the global variable *NONNUMBER-TYPES* before
compiling or loading Screamer. Screamer will automatically generate all 
associated functions and generator functions for every type in the list. This
is the best approach if you want the new type to be available everywhere Screamer
is used.

2.Macro Method (Recommended for project-specific types):
In your own project, you can manually call the macros (DEFINE-SCREAMER-TYPE ...)
and (DEFINE-SCREAMER-GENERATOR-FUNCTION ...) from within the SCREAMER package
before loading or compiling any code that uses these functions. This is useful
if users want to define types only for a specific project or session.

The generated functions follow a consistent naming convention based on the type name,
e.g. the type LIST generates LISTPV, A-LISTV; the type ARRAY generates ARRAYPV,
AN-ARRAYV, etc.

Example:

Include the type or types in the global variable list *NONNUMBER-TYPES*:

(defparameter-compile-time *nonnumber-types*
  '(hash-table ...) "...")

Or manually define the type:

(in-package :screamer)
(define-screamer-type hash-table)
(define-screamer-generator-function hash-table)

This will generate the following functions:
VARIABLE-POSSIBLY-HASH-TABLE?
VARIABLE-HASH-TABLE?
VARIABLE-NONHASH-TABLE?
KNOWN?-HASH-TABLEPV
KNOWN?-NOTV-HASH-TABLEPV
ASSERT!-HASH-TABLEPV
ASSERT!-NOTV-HASH-TABLEPV
HASH-TABLEPV (EXPORTED)
A-HASH-TABLEV (EXPORTED)

The user can now use (KNOWN? (HASH-TABLEPV X)), (ASSERT! (HASH-TABLEPV X)) and
(DECIDE (HASH-TABLEPV)), which will call the specialized lifted functions.

The generator function A-HASH-TABLEV can be used to create variables of type
hash-table.

Currently the version 4.0.1 was tested on the following LISP implementations:
 - SBCL 2.5.8
 - Allegro Common LISP 11.0
 - LispWorks 8.0.1
 - Clozure Common LISP 1.13
