;;;; Written by:
;;;;
;;;;   Jeffrey Mark Siskind (Department of Computer Science, University of Toronto)
;;;;   David Allen McAllester (MIT Artificial Intelligence Laboratory)
;;;;
;;;; Copyright 1991 Massachusetts Institute of Technology. All rights reserved.
;;;; Copyright 1992, 1993 University of Pennsylvania. All rights reserved.
;;;; Copyright 1993 University of Toronto. All rights reserved.
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;;;; this software and associated documentation files (the "Software"), to deal in
;;;; the Software without restriction, including without limitation the rights to
;;;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;;;; the Software, and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:
;;;;
;;;; The above copyright and authorship notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;;;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;;;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;;;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :screamer
  (:shadow :defun :multiple-value-bind :y-or-n-p :variable)
  (:use :cl)
  (:nicknames :s)
  (:export #:either
           #:fail
           #:local
           #:global
           #:for-effects
           #:multiple-value-call-nondeterministic
           #:one-value
           #:possibly?
           #:necessarily?
           #:all-values
           #:ith-value
           #:n-values
           #:print-values
           #:nondeterministic-function?
           #:funcall-nondeterministic
           #:apply-nondeterministic
           #:mapcar-nondeterministic
           #:map-nondeterministic
           #:lambda-nondeterministic
           #:unwind-trail
           #:trail
           #:purge
           #:unwedge-screamer
           #:local-output
           #:a-boolean
           #:an-integer
           #:an-integer-above
           #:an-integer-below
           #:an-integer-between
           #:a-rational
           #:a-rational-above
           #:a-rational-below
           #:a-rational-between
           #:a-ratio
           #:a-ratio-above
           #:a-ratio-below
           #:a-ratio-between
           #:a-member-of
           #:a-random-member-of		   
           #:when-failing
           #:count-failures
           #:boolean
           #:booleanp
           #:make-variable
           #:numberpv
           #:ratiopv
           #:rationalpv
           #:floatpv
           #:realpv
           #:integerpv
           #:booleanpv
           #:memberv
           #:all-differentv
           #:assert!
           #:known?
           #:decide
           #:=v
           #:<v
           #:<=v
           #:>v
           #:>=v
           #:/=v
           #:a-booleanv
           #:an-integerv
           #:an-integer-abovev
           #:an-integer-belowv
           #:an-integer-betweenv
           #:a-rationalv
           #:a-rational-abovev
           #:a-rational-belowv
           #:a-rational-betweenv
           #:a-ratiov
           #:a-ratio-abovev
           #:a-ratio-belowv
           #:a-ratio-betweenv
           #:a-floatv
           #:a-float-abovev
           #:a-float-belowv
           #:a-float-betweenv
           #:a-realv
           #:a-real-abovev
           #:a-real-belowv
           #:a-real-betweenv
           #:a-numberv
           #:a-member-ofv
           #:a-random-member-ofv
           #:n-variables
           #:n-lists-of-variables
           #:notv
           #:andv
           #:orv
           #:count-trues
           #:count-truesv
           #:+v
           #:-v
           #:*v
           #:/v
           #:minv
           #:maxv
           #:funcallv
           #:applyv
           #:equalv
           #:bound?
           #:value-of
           #:ground?
           #:apply-substitution
           #:linear-force
           #:divide-and-conquer-force
           #:random-force
           #:static-ordering
           #:domain-size
           #:range-size
           #:reorder
           #:solution
           #:best-value
           #:optimize-value
           #:template
           #:define-screamer-package
           #:*screamer-version*
           #:*dynamic-extent?*
           #:*iscream?*
           #:*minimum-shrink-ratio*
           #:*maximum-discretization-range*
           #:*strategy*
           #:*maximum-random-domain-size*
           #:define-screamer-type
           #:define-screamer-generator-function
           #:*nonnumber-types*))

(in-package :screamer)
;;; Avoid warnings when compiling screamer.lisp in LispWorks
#+lispworks(declaim (declaration magic))
