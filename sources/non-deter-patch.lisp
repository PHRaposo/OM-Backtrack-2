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

(defvar *backtrack-debug* nil)

(defmethod nondeter-omlispfun? ((self OMBoxlispCall))
 (let* ((funname (reference self)) ;(car (gen-code self 0))) ;fix 20.06.2024
        (record (screamer::get-function-record funname)))
  (not (screamer::function-record-deterministic? record))))
  
(defmethod non-deter-patch? ((self OMLispPatchAbs) &optional patches)
(declare (ignore patches))
(let* ((exp (get-lisp-exp (lisp-exp self)))
         (non-deter? (multiple-value-list (ignore-errors (not (s::function-record-deterministic?
                                                 (s::get-function-record
                                                  (eval `(screamer::defun ,(intern (string (code self)) :om)
                                                             ,.(cdr exp))))))))))
 (if (and (= (length non-deter?) 2) ;<== FROM SCREAMER-PLUS (CAREFULLY)
	   (null (car non-deter?))
	   (typep (second non-deter?) (find-class 'error)))
   nil
  (car non-deter?))))

(defmethod non-deter-patch? ((self OMPatch) &optional patches) 
 (let* ((patches (x-append self patches))
        (boxes (boxes self))
	    (screamerboxes (find-class-boxes boxes 'screamerboxes)) ;<== SCREAMER FUNCTIONS
	    (screamer-valuation-boxes (find-class-boxes boxes 'screamer-valuation-boxes)) ;<== SCREAMER VALUATION
        (lispfuns (find-class-boxes boxes 'omboxlispcall)) ;<== OMLISPFUN
        (sub-patches  (x-append (find-class-boxes boxes 'omboxpatch) ;<== COLLECT OMBOXPATCH (29.12.2024)
		                        (find-class-boxes boxes 'omboxabspatch))) ;<== OMBOXABSPATCH (SUB PATCHES)
        (non-deter-sub-patch? (not (null (position t (mapcar #'(lambda (x)
													  (let ((ref (reference x)))
													   (if (member ref patches :test #'equal)
										                    nil
		                                                   (non-deter-patch? ref patches))))
													  sub-patches))))))	
  (if (or screamerboxes (some #'nondeter-omlispfun? lispfuns) non-deter-sub-patch? screamer-valuation-boxes) 
       t
       nil)))

(defmethod om-draw-contents :after ((self patch-icon-box))
  (when (non-deter-patch? (reference (object (om-view-container self))))
   (om-with-fg-color self *om-pink-color*
    (om-with-font (om-make-font "Courier" (* *icon-size-factor* 34))
      (om-draw-char (- (round (w self) 2) (* *icon-size-factor* 10)) 
		            (+ (round (h self) 2) (* *icon-size-factor* 10)) 
	    #\?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; omNG-box-value - OMBoxPatch

(defmethod omNG-box-value ((self OMBoxPatch) &optional (num-out 0))
(when *backtrack-debug* (print (format nil "COMPILED?: ~A" (compiled? (reference self))))) 
(handler-bind ((error #'(lambda (c)
                          (when *msg-error-label-on*
                            (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                     (om-report-condition c))
                                               :size (om-make-point 300 200))
                            (clear-after-error self)
                            (om-abort)))))
   (cond  	   	           			  				             				 	 			
    ((and (equal (allow-lock self) "x") (value self))
     (nth num-out (value self)))	 
		 
	((and (equal (allow-lock self) "o")
	 (setf (value self) (list (reference self))) (car (value self))))
	 	 
    ((equal (allow-lock self) "l")
     ;(unless (compiled? (reference self))
       (if (and (lisp-exp-p (reference self)) (editorframe self))
         (progn (when *backtrack-debug* (print "OMLISP-LAMBDA-PATCH COMPILED!"))
		  (compile-without-close (editorframe self)))          
          (progn (when *backtrack-debug* (print "LAMBDA-PATCH COMPILED!"))
		   (compile-patch (reference self)))
		  ) ;)				 	 				  
         (setf (value self) ;;; TEST
                (list (special-lambda-value self (intern (string (code (reference self))) :om))))
         (when *backtrack-debug* (print "SETF LAMBDA VALUE: TEST"))
		 (car (value self))		 
		 )
		 		  
    ((and (equal (allow-lock self) "&") (ev-once-p self)) 
     (nth num-out (value self)))
	 
    (t ;(unless (compiled? (reference self))
           (if (and (lisp-exp-p (reference self)) (editorframe (reference self))) 
             (progn (compile-without-close (editorframe (reference self))) 
				(when *backtrack-debug* (print "OMLISPPATCH COMPILED!")))
             (progn (compile-patch (reference self)) 
			  (when *backtrack-debug* (print "PATCH COMPILED!"))))
			  ;)
		(let* ((args  (mapcar #'(lambda (input) 
                                 (omNG-box-value  input)) (inputs self)))
              (rep nil))
       
			 (if (non-deter-patch? (reference self))
			     (setf rep (multiple-value-list (eval `(,(intern (string (code (reference self))) :om) 
                                           ,.(loop for item in args collect `',item)))))
		         (setf rep (multiple-value-list (apply (intern (string (code (reference self))) :om) args))))
			 			 
         (when (equal (allow-lock self) "&")
           (setf (ev-once-p self) t)
           (setf (value self) rep))
         (when (equal (allow-lock self) "x")
           (setf (value self) rep))
           ;;;; TEST
           (when (equal (allow-lock self) nil)
		   (when *backtrack-debug* (print "ALLOW-LOCK NIL: SETF VALUE - TEST"))
             (setf (value self) rep)
			  )
           ;;;;	 	   
             (nth num-out rep))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMPILE-PATCH

(defmethod compile-patch ((self OMPatch)) 
 "Generation of lisp code from the graphic boxes."
	;(unless (compiled? self)
	(if (lisp-exp-p self)
	    (if (non-deter-patch? self)
            (compile (eval `(screamer::defun ,(intern (string (code self)) :om)
                             ,.(cdr (get-lisp-exp (lisp-exp self))))))
	        (compile (eval `(defun ,(intern (string (code self)) :om)
	                         ,.(cdr (get-lisp-exp (lisp-exp self))))))) 									
	(let* ((boxes (boxes self))
	   (temp-out-box (find-class-boxes boxes 'OMtempOut))
	   (self-boxes (patch-has-temp-in-p self)) 
	   (out-box (find-class-boxes boxes 'OMout))
	   (in-boxes (find-class-boxes boxes 'OMin))
	   (out-symb (code self))
	   (oldletlist *let-list*)
	   (oldlambdacontext *lambda-context*)		 
	  symbols nondeterministic-context? body)
	  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
	  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
	  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
	  (setf *let-list* nil)	 
	  (cond ((non-deter-patch? self)
;>========== *LET-LIST* IN NONDETERMINISTIC CONTEXTS - MULTIPLE OUTPUTS ==========<; 
		     (setf body `(values ,.(mapcar #'(lambda (theout)
                                                                       (let ((theinputs (loop for i in (inputs theout)
	                                                                                                collect (connected? i))))    
                                                                       (if (screamer-valuation-boxes-p (caar theinputs))
                                                                           (progn (setf nondeterministic-context? t)
                                                                                      (gen-valuation-code (caar theinputs) (cadar theinputs)))
                                                                           (gen-code theout 0))))
                                                        out-box)))
				;(print `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		  		;   ,body)) ;<== CHECK CODE
		     (if nondeterministic-context? 
                        (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		  		   ,body)) 
                        (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		  		   (let* ,(reverse *let-list*) ,body)))))
	         ;(setf body `(values ,.(mapcar #'(lambda (theout) (gen-code theout 0)) out-box)))
	  	 ; 	 (eval `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
	  	 ; 	     	(let* ,(reverse *let-list*) ,body))))	
;>==============================================================================<; 											   													   
  		    (t (setf body `(values ,.(mapcar #'(lambda (theout)
									   (gen-code theout 0)) out-box)))					   
  			   (eval `(defun ,(intern (string out-symb) :om)  (,.symbols)
  			   	       (let* ,(reverse *let-list*) ,body))))) 						   						   								   			  	  				 						 						 		 	 
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)
		   ))	
	(setf (compiled? self) t))
	;)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;; PATCH-CODE

(defmethod gen-patch-code ((self OMPatch)) 
	"Prints the lisp code from a clone of a patch in itself mode."
	(let ((patch-clone (clone self)))
	(if (lisp-exp-p patch-clone)		
		(if (non-deter-patch? self)
	        `(screamer::defun ,(intern (string (code patch-clone)) :om)
	                        ,.(cdr (get-lisp-exp (lisp-exp patch-clone))))
		    `(defun ,(intern (string (code self)) :om)
		                    ,.(cdr (get-lisp-exp (lisp-exp patch-clone))))) 					
	(let* ((boxes (boxes patch-clone))
	   (temp-out-box (find-class-boxes boxes 'OMtempOut))
	   (self-boxes (patch-has-temp-in-p patch-clone)) 
	   (out-box (find-class-boxes boxes 'OMout))
	   (in-boxes (find-class-boxes boxes 'OMin))
	   (out-symb (code patch-clone))
	   (oldletlist *let-list*)
	   (oldlambdacontext *lambda-context*)		 
	  symbols body nondeterministic-context? patch-code)
	  (setf out-box (list+ temp-out-box (sort out-box '< :key 'indice)))
	  (setf in-boxes (list+ self-boxes (sort in-boxes '< :key 'indice)))
	  (setf symbols (mapcar #'(lambda (thein) (setf (in-symbol thein) (gensym))) in-boxes))
	  (setf *let-list* nil) 
	  (cond ((non-deter-patch? self) 
		     (setf body `(values ,.(mapcar #'(lambda (theout)
                                                                       (let ((theinputs (loop for i in (inputs theout)
	                                                                                                collect (connected? i))))
                                                                        
                                                                       (if (screamer-valuation-boxes-p (caar theinputs))
                                                                           (progn (setf nondeterministic-context? t)
                                                                                      (gen-valuation-code (caar theinputs) (cadar theinputs)))
                                                                           (gen-code theout 0))))
                                                        out-box)))
		     (setf patch-code (if nondeterministic-context? 
                                                   `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		  		  	     	     ,body)
                                                  `(screamer::defun ,(intern (string out-symb) :om)  (,.symbols)
		  		  	     	    (let* ,(reverse *let-list*) ,body)))))
								
  		    (t (setf body `(values ,.(mapcar #'(lambda (theout)
				                      (gen-code theout 0)) out-box)))					   
  			   (setf patch-code `(defun ,(intern (string out-symb) :om)  (,.symbols)
  			   	       (let* ,(reverse *let-list*) ,body))))) 																	   													  
	  (setf *let-list* oldletlist)
	  (setf *lambda-context* oldlambdacontext)    
	   patch-code ;===> returns the function code
	 )))	
	)
	 	   
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;OMLispPatch

(defun compile-lisp-patch-fun (patch)
  (if (get-lisp-exp (lisp-exp patch))
      (if (non-deter-patch? patch)
          (eval `(screamer::defun ,(intern (string (code patch)) :om)
                   ,.(cdr (get-lisp-exp (lisp-exp patch)))))
          (eval `(defun ,(intern (string (code patch)) :om)
                  ,.(cdr (get-lisp-exp (lisp-exp patch))))))
    (eval `(defun ,(intern (string (code patch)) :om) () nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Evaluation
(defmethod special-lambda-value ((self OMBoxPatch) symbol)
   "Eval a box in lambda mode."
       (multiple-value-bind (nesymbs args) (get-args-eval-currry self)
         (if (non-deter-patch? (reference self))
               (eval `#'(lambda ,(reverse nesymbs)
                              (,symbol ,.args)))
               (eval `#'(lambda ,(reverse nesymbs)
                (apply ',symbol (list ,.args)))))					     
 ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;code generation

(defmethod curry-lambda-code ((self OMBoxPatch) symbol)
   "Lisp code generetion for a box in lambda mode."
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
        (if (non-deter-patch? (reference self))
             `#'(lambda ,(reverse nesymbs)
                  (,symbol ,.args))
             `#'(lambda ,(reverse nesymbs)
                  (apply ',symbol (list ,.args)))))
 
       (setf *lambda-context* oldlambdacontext)
   
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;TODO (IF NEEDED)
;;; 
;;; => OMLOOP (???)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

