;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION (RESTORATION OF OM-BACKTRACK)
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD
;;;
;;; OM-BACKTRACK 2.0
;;; Copyright 2024 PAULO HENRIQUE RAPOSO
;;;

(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; "?" - NONDETERMINISTIC PATCH (NEW VERSION FOR BACKTRACK 2.0)
;;;
;;; A patch can play three roles relative to Screamer's nondeterminism:
;;;
;;;   (1) Pure deterministic: contains no Screamer constructs.  OM
;;;       core's COMPILE-PATCH primary handles it correctly; we
;;;       delegate via CALL-NEXT-METHOD from the :AROUND override.
;;;
;;;   (2) Non-deterministic: at least one OUTBOX exposes a Screamer
;;;       choice point directly to the caller.  Compile via
;;;       SCREAMER::DEFUN so the calling continuation propagates
;;;       through CPS.
;;;
;;;   (3) Deterministic with internal nondeterministic context:
;;;       every OMout is wired to a SCREAMER-VALUATION box (ALL-VALUES,
;;;       ONE-VALUE, etc.), so the patch's external interface returns
;;;       ordinary values, but the body must establish a nondet
;;;       context per outbox via GEN-VALUATION-CODE.  Compile via
;;;       plain DEFUN.
;;;
;;; Mixed patches (some outboxes wrapped in valuation, others
;;; exposed) fall under (2): SCREAMER::DEFUN outer, body chooses
;;; GEN-VALUATION-CODE or GEN-CODE per outbox individually -- the
;;; body generation is per-outbox regardless of the patch-level
;;; classification.

;;; ----------------------------------------------------------------
;;; Helpers shared by predicates and code generators.

(defmethod nondeter-omlispfun? ((self OMBoxlispCall))
  (let* ((funname (reference self))
         (record (screamer::get-function-record funname)))
    (not (screamer::function-record-deterministic? record))))

(defmethod outboxes-valuation-status ((self OMPatch))
  "Return one generalised boolean per OMout in SELF, T when the
outbox's first connected input is a SCREAMER-VALUATION box.  Used
by both NON-DETER-PATCH? and NONDETERMINISTIC-CONTEXT-PATCH?, and
mirrored inside the body generation that picks GEN-VALUATION-CODE
vs GEN-CODE per outbox."
  (mapcar #'(lambda (theout)
              (let ((theinputs (loop for i in (inputs theout)
                                     collect (connected? i))))
                (and theinputs
                     (screamer-valuation-boxes-p (caar theinputs)))))
          (find-class-boxes (boxes self) 'OMout)))

;;; ----------------------------------------------------------------
;;; Predicates exposing the three regimes.

(defmethod non-deter-patch? ((self OMLispPatchAbs) &optional patches)
  (declare (ignore patches))
  (let ((exp (get-lisp-exp (lisp-exp self))))
    (handler-case
        (not (s::function-record-deterministic?
              (s::get-function-record
               (eval `(screamer::defun ,(intern (string (code self)) :om)
                                       ,.(cdr exp))))))
      (error () nil))))

(defmethod non-deter-patch? ((self OMPatch) &optional patches)
  "T iff SELF needs SCREAMER::DEFUN compilation: it uses Screamer
stuff (a Screamer function box, a non-deterministic OMBoxLispCall,
or a non-deterministic sub-patch) AND at least one OMout exposes
that nondeterminism directly (i.e. is not wrapped in a SCREAMER-
VALUATION box)."
  (let* ((patches (x-append self patches))
         (boxes (boxes self))
         (screamerboxes (find-class-boxes boxes 'screamerboxes))
         (lispfuns (find-class-boxes boxes 'omboxlispcall))
         (sub-patches (x-append (find-class-boxes boxes 'omboxpatch)
                                (find-class-boxes boxes 'omboxabspatch)))
         (non-deter-sub-patch?
          (some #'(lambda (x)
                    (let ((ref (reference x)))
                      (and (not (member ref patches :test #'equal))
                           (non-deter-patch? ref patches))))
                sub-patches)))
    (and (some #'null (outboxes-valuation-status self))
         (or screamerboxes
             (some #'nondeter-omlispfun? lispfuns)
             non-deter-sub-patch?))))

(defmethod nondeterministic-context-patch? ((self OMLispPatchAbs))
  "Lisp patches do not have OMout boxes wired to valuation; they
never fall into the deterministic-with-nondet-context regime."
  nil)

(defmethod nondeterministic-context-patch? ((self OMPatch))
  "T iff at least one OMout in SELF is wired to a SCREAMER-
VALUATION box.  Independent of NON-DETER-PATCH?: a mixed patch
(some outboxes wrapped, some not) is both non-deter and nondet-
context.  Used inside the COMPILE-PATCH :AROUND to decide whether
OM core's primary (which only emits GEN-CODE) is sufficient or
whether we need to override body generation to use GEN-VALUATION-
CODE per valuation-wrapped outbox."
  (some #'identity (outboxes-valuation-status self)))

;;; ----------------------------------------------------------------
;;; Visual indicator on the patch icon.

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (non-deter-patch? (reference (object (om-view-container self))))
    (om-with-fg-color self *om-pink-color*
      (om-with-font (om-make-font "Courier"
                                  (* *icon-size-factor*
                                     (if (>= cl-user::*version* 8) 30 34)))
        (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10))
                      (+ (round (h self) 2) (* *icon-size-factor* 10))
                      #\?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; omNG-box-value - OMBoxPatch
;;;
;;; The non-deter branch quotes its arguments and uses EVAL so the
;;; calling continuation flows through CPS-converted code; the
;;; deterministic branch uses the standard APPLY of OM core.  Kept as
;;; a primary-method override (rather than :AROUND) because the
;;; differing emission is buried inside the lock-mode CONDs.

(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
  (handler-bind ((error #'(lambda (c)
                            (when *msg-error-label-on*
                              (om-message-dialog
                               (string+ "Error while evaluating the box "
                                        (string (name self)) " : "
                                        (om-report-condition c))
                               :size (om-make-point 300 200))
                              (clear-after-error self)
                              (om-abort)))))
    (cond
      ((and (equal (allow-lock self) "x") (value self))
       (nth num-out (value self)))
      ((and (equal (allow-lock self) "o")
            (setf (value self) (list (reference self)))
            (car (value self))))
      ((equal (allow-lock self) "l")
       (unless (compiled? (reference self))
         (if (and (lisp-exp-p (reference self)) (editorframe self))
             (compile-without-close (editorframe self))
             (compile-patch (reference self))))
       (setf (value self)
             (list (special-lambda-value
                    self
                    (intern (string (code (reference self))) :om))))
       (car (value self)))
      ((and (equal (allow-lock self) "&") (ev-once-p self))
       (nth num-out (value self)))
      (t (if (and (lisp-exp-p (reference self)) (editorframe (reference self)))
             (compile-without-close (editorframe (reference self)))
             (compile-patch (reference self)))
         (let* ((args (mapcar #'(lambda (input)
                                  (omNG-box-value input))
                              (inputs self)))
                (rep nil))
           (if (non-deter-patch? (reference self))
               (setf rep
                     (multiple-value-list
                      (eval `(,(intern (string (code (reference self))) :om)
                               ,.(loop for item in args collect `',item)))))
               (setf rep
                     (multiple-value-list
                      (apply (intern (string (code (reference self))) :om)
                             args))))
           (when (equal (allow-lock self) "&")
             (setf (ev-once-p self) t)
             (setf (value self) rep))
           (when (equal (allow-lock self) "x")
             (setf (value self) rep))
           (when (equal (allow-lock self) nil)
             (setf (value self) rep))
           (nth num-out rep))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-PATCH and GEN-PATCH-CODE -- shared body generation.
;;;
;;; COMPILE-PATCH is dispatched as an :AROUND on OMPatch so the
;;; deterministic case (1) delegates to OM core's primary via
;;; CALL-NEXT-METHOD, with no replication of OM-side logic.  The
;;; :AROUND only intervenes for cases (2) and (3), which need a
;;; different OUTER (DEFUN vs SCREAMER::DEFUN) and a body that
;;; conditionally uses GEN-VALUATION-CODE per outbox.
;;;
;;; GEN-PATCH-CODE is OM-Backtrack-only -- no OM core counterpart --
;;; so all three cases must build the form explicitly.

(defun screamer-aware-compile-patch (self outer-symbol)
  "Replicates OM core's COMPILE-PATCH (OMPatch) but emits OUTER-
SYMBOL (DEFUN or SCREAMER::DEFUN) and uses GEN-VALUATION-CODE for
outboxes wired to a SCREAMER-VALUATION box, GEN-CODE otherwise."
  (unless (compiled? self)
    (if (lisp-exp-p self)
        (compile (eval `(,outer-symbol
                          ,(intern (string (code self)) :om)
                          ,.(cdr (get-lisp-exp (lisp-exp self))))))
        (let* ((boxes (boxes self))
               (temp-out-box (find-class-boxes boxes 'OMtempOut))
               (self-boxes (patch-has-temp-in-p self))
               (out-box (find-class-boxes boxes 'OMout))
               (in-boxes (find-class-boxes boxes 'OMin))
               (out-symb (code self))
               (oldletlist *let-list*)
               (oldlambdacontext *lambda-context*)
               symbols body)
          (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
          (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
          (setf symbols (mapcar #'(lambda (thein)
                                    (setf (in-symbol thein) (gensym)))
                                in-boxes))
          (setf *let-list* nil)
          (setf body
                `(values
                  ,.(mapcar
                     #'(lambda (theout)
                         (let ((theinputs (loop for i in (inputs theout)
                                                collect (connected? i))))
                           (cond ((screamer-valuation-boxes-p (caar theinputs))
                                  (gen-valuation-code (caar theinputs)
                                                      (cadar theinputs)))
                                 (t (gen-code theout 0)))))
                     out-box)))
          (eval `(,outer-symbol
                   ,(intern (string out-symb) :om) (,.symbols)
                   (let* ,(reverse *let-list*) ,body)))
          (setf *let-list* oldletlist)
          (setf *lambda-context* oldlambdacontext)))
    (setf (compiled? self) t)))

(defmethod compile-patch :around ((self OMPatch))
  "Dispatch over the three regimes.  Case (1) -- pure deterministic
-- delegates to OM core via CALL-NEXT-METHOD."
  (cond
    ((non-deter-patch? self)
     (screamer-aware-compile-patch self 'screamer::defun))
    ((nondeterministic-context-patch? self)
     (screamer-aware-compile-patch self 'defun))
    (t
     (call-next-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PATCH-CODE -- pure code-emission, no eval.

(defun screamer-aware-gen-patch-code (patch-clone outer-symbol)
  "Builds the source form (OUTER-SYMBOL NAME (ARGS) (LET* ... BODY))
for PATCH-CLONE, using GEN-VALUATION-CODE for valuation-wrapped
outputs.  Returns the form without evaluating it."
  (if (lisp-exp-p patch-clone)
      `(,outer-symbol ,(intern (string (code patch-clone)) :om)
                       ,.(cdr (get-lisp-exp (lisp-exp patch-clone))))
      (let* ((boxes (boxes patch-clone))
             (temp-out-box (find-class-boxes boxes 'OMtempOut))
             (self-boxes (patch-has-temp-in-p patch-clone))
             (out-box (find-class-boxes boxes 'OMout))
             (in-boxes (find-class-boxes boxes 'OMin))
             (out-symb (code patch-clone))
             (oldletlist *let-list*)
             (oldlambdacontext *lambda-context*)
             symbols body patch-code)
        (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
        (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
        (setf symbols (mapcar #'(lambda (thein)
                                  (setf (in-symbol thein) (gensym)))
                              in-boxes))
        (setf *let-list* nil)
        (setf body
              `(values
                ,.(mapcar
                   #'(lambda (theout)
                       (let ((theinputs (loop for i in (inputs theout)
                                              collect (connected? i))))
                         (cond ((screamer-valuation-boxes-p (caar theinputs))
                                (gen-valuation-code (caar theinputs)
                                                    (cadar theinputs)))
                               (t (gen-code theout 0)))))
                   out-box)))
        (setf patch-code
              `(,outer-symbol
                ,(intern (string out-symb) :om) (,.symbols)
                (let* ,(reverse *let-list*) ,body)))
        (setf *let-list* oldletlist)
        (setf *lambda-context* oldlambdacontext)
        patch-code)))

(defmethod gen-patch-code ((self OMPatch))
  "Prints the lisp code from a clone of a patch in itself mode.
No OM core counterpart, so all three cases must emit form here.
Cases (1) and (3) share the same OUTER (DEFUN); (2) uses
SCREAMER::DEFUN.  Body generation is uniform across all three --
GEN-VALUATION-CODE only fires for outboxes wired to SCREAMER-
VALUATION boxes, so case (1) ends up with pure GEN-CODE bodies."
  (let ((patch-clone (clone self)))
    (cond
      ((non-deter-patch? self)
       (screamer-aware-gen-patch-code patch-clone 'screamer::defun))
      (t
       (screamer-aware-gen-patch-code patch-clone 'defun)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;OMLispPatch
;;;
;;; COMPILE-LISP-PATCH-FUN is a DEFUN, not a method, so it cannot use
;;; CALL-NEXT-METHOD; OM-Backtrack must replace OM core's version
;;; wholesale.  Lisp patches have no OMout boxes, hence case (3) does
;;; not apply; only cases (1) and (2) need handling.

(defun compile-lisp-patch-fun (patch)
  (if (get-lisp-exp (lisp-exp patch))
      (cond ((non-deter-patch? patch)
             (eval `(screamer::defun
                     ,(intern (string (code patch)) :om)
                      ,.(cdr (get-lisp-exp (lisp-exp patch))))))
            (t
             (eval `(defun
                     ,(intern (string (code patch)) :om)
                      ,.(cdr (get-lisp-exp (lisp-exp patch)))))))
      (eval `(defun ,(intern (string (code patch)) :om) () nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Evaluation -- lambda mode
;;;
;;; OM core defines SPECIAL-LAMBDA-VALUE on OMBoxcall (OMBoxPatch's
;;; parent) emitting (LAMBDA ... (APPLY ',SYMBOL (LIST ,.ARGS))).
;;; OM-Backtrack's only divergence is for non-deter patches, where
;;; the body must be a direct (,SYMBOL ,.ARGS) so the calling
;;; continuation flows through CPS.  Implemented as :AROUND so case
;;; (1) delegates to OM core via CALL-NEXT-METHOD.

(defmethod special-lambda-value :around ((self OMBoxPatch) symbol)
  (if (non-deter-patch? (reference self))
      (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
        (eval `#'(lambda ,(reverse nesymbs)
                   (,symbol ,.args))))
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;code generation -- lambda-mode source emission
;;;
;;; Same delegation pattern as SPECIAL-LAMBDA-VALUE: OMBoxcall's
;;; primary already emits the deterministic apply-form; we only
;;; intervene on the non-deter branch.

(defmethod curry-lambda-code :around ((self OMBoxPatch) symbol)
  (if (non-deter-patch? (reference self))
      (let ((nesymbs nil)
            (oldlambdacontext *lambda-context*))
        (setf *lambda-context* t)
        (unwind-protect
            (let ((args (mapcan #'(lambda (input)
                                    (let ((a (if (connected? input)
                                                 (gen-code input 0)
                                                 (let ((newsymbol (gensym)))
                                                   (push newsymbol nesymbs)
                                                   newsymbol))))
                                      (if (keyword-input-p input)
                                          (list (value input) a)
                                          (list a))))
                                (inputs self))))
              `#'(lambda ,(reverse nesymbs)
                   (,symbol ,.args)))
          (setf *lambda-context* oldlambdacontext)))
      (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;TODO (IF NEEDED)
;;;
;;; => OMLOOP (???)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
