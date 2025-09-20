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

(in-package :screamer)

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


;(defun list-of-chords-in-simple (l dom)
;  (if (null l) nil
;      (cons (a-chord-in (car l) dom) 
;            (list-of-chords-in (cdr l) dom))))

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

(defun list-of-chords-in (l dom &optional cont)
  (cond ((null cont)
         (if (null l) nil
            (cons (a-chord-in (car l) dom) 
            (list-of-chords-in (cdr l) dom))))
         ;(list-of-chords-in-simple l dom))
        ((functionp cont) (list-of-chords-in-cont l dom cont))
        ((listp cont) (list-of-chords-in-lcont l dom cont))
        (t (print `(La contrainte doit etre function, nil ou list)))))