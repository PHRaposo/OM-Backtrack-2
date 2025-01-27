;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; REVISED VERSION
;;; Copyright 2024 PAULO HENRIQUE RAPOSO AND KARIM HADDAD

(in-package :om)

;(defvar *screamer-listener-size* (om-make-point 600 300));(om-make-point 400 200))

(defclass non-deter-window (EditorWindow om-dialog)
  ((value-item :initform nil :accessor value-item)))

(defmethod oa::set-not-resizable ((self non-deter-window) &optional
width height) nil)

;(defmethod update-di-size ((self om-text-edit-view) container)
;  (om-set-view-position self #+win32(om-make-point 12 12) #-win32(om-make-point 12 10))
;  (om-set-view-size self (om-subtract-points (om-view-size container) #+win32(om-make-point 28 24) #-win32(om-make-point 28 20))))

#|
(defmethod om-set-view-size ((self non-deter-window) size) 
   (declare (ignore size))
   (call-next-method) 
   ;(setf *screamer-listener-size* (om-view-size self))
   (when (value-item self)
     (om-set-view-size (value-item self) (om-make-point (- (w self) 25) (- (h self) 50)))))
|#

(defmethod oa::om-resize-callback ((self non-deter-window) x y w h)
    (let* ((value (value-item self)))
	;(print (list "resizef" x y w h))  
(when value
        (om-set-view-size value (om-make-point (- w 40) (- h 40))))
    ))

(defun non-determinise-listener (value)
  (let (dialog value-item-view)
    (setf value-item-view
          (cond 
           ((omclass-p (class-of (class-of value)))
            (setf dialog (om-make-window 'non-deter-window 
                                         :window-title "Non Deterministic Listener"
                                         :position :centered 
                                         ;:window-show nil
                                         :size (om-make-point 600 350) ;*screamer-listener-size*
                                         ;:font (om-make-font "Arial" 12 :mode :srcor :style :plain)
                                         :bg-color (om-make-color 0.875 0.875 0.875)))
            (cond
             ((Class-has-editor-p value)
              (setf (value-item dialog) 
                    (setf (editor dialog) (om-make-view (get-editor-class value)
                                                        :ref nil
                                                        :container dialog
                                                        :object value
                                                        :position (om-make-point 25 35) 
                                                        :size (om-make-point (- (w dialog) 25) (- (h dialog) 25))))))
             (t (setf (value-item dialog) 
                      (setf (editor dialog) 
                            (let* ((instance (omNG-make-new-instance value "instance"))
                                   (editor (om-make-view 'InstanceEditor
                                                         :ref nil
                                                         :container dialog
                                                         :object instance
                                                         :position (om-make-point 25 35) 
                                                         :size (om-make-point (- (w dialog) 25) (- (h dialog) 25))))
                                   (slot-boxes (make-slots-ins-boxes instance)))
                              (setf (presentation editor) 0)
                              (mapc #'(lambda (frame)
                                        (omG-add-element (panel editor) frame)) slot-boxes)
                              editor))))))
           (t (setf dialog (om-make-window 'non-deter-window 
                                           :window-title "Non Deterministic Listener"
                                           :position :centered 
                                           :window-show nil
                                           :size (om-make-point 600 400) 
                                           :bg-color (om-make-color 0.875 0.875 0.875)))	      
              (om-make-view 'om-text-edit-view
                            :save-buffer-p t
                            :scrollbars :v
                            :text (format nil "~D" value)
                            :wrap-p t
                            :size (om-make-point 560 350)
                            :position (om-make-point  25 35)))))

    (om-add-subviews dialog value-item-view  
                     (om-make-dialog-item 'om-static-text (om-make-point 25 10) (om-make-point 420 20) "DO YOU WANT ANOTHER SOLUTION?")
					 
                     (om-make-dialog-item 'om-button (om-make-point 250 5) (om-make-point 62 20) "No" 
                                          :di-action (om-dialog-item-act item
                                                      (let ((item item))
													   (declare (ignore item))
													   (om-return-from-modal-dialog dialog nil))))
                     (om-make-dialog-item 'om-button (om-make-point 320 5) (om-make-point 58 18) "Yes" 
                                          :di-action (om-dialog-item-act item
                                                     (let ((item item))
													  (declare (ignore item))
													  (om-return-from-modal-dialog dialog t)))
                                          :default-button t))
 
  (if (scoreeditor-p value)
       (let ((score-class (class-name (class-of value)))) 
        (cond ((or (equal score-class 'poly)
	                 (equal score-class 'multi-seq))
		   (let* ((inside (inside value))
                           (staves (mapcar #'correct-listener-staff inside)))
		    (loop for staff in staves
			     for x from 0
			     do (setf (nth x (staff-sys (panel (editor dialog)))) (get-staff-system staff)))
		    (update-panel (panel (editor dialog)))
		    (non-deter-modal-dialog dialog)))			
                (t (progn (change-system (panel (editor dialog)) (correct-listener-staff value)) 
                               (non-deter-modal-dialog dialog)))))
     (non-deter-modal-dialog dialog)
    )
))

;;;(non-determinise-listener 0)

(defun non-deter-modal-dialog (dialog &optional (owner nil))
 (declare (ignore owner))
 (oa::update-for-subviews-changes dialog t)
  ;(print (list (vsubviews self)))
 (capi::display-dialog dialog))
  ;:owner (or owner (om-front-window) (capi:convert-to-screen)) 
  ;                        :position-relative-to :owner :x (vx dialog) :y (vy dialog)))

(defun correct-listener-staff (value)
 (let ((class-of-value (class-name (class-of value))))
 (cond 
  ((equal class-of-value 'note)
     (let ((midics (midic value)))
      (get-staff-from-midics midics)))

  ((or (equal class-of-value 'chord) 
         (equal class-of-value 'chord-seq))
     (let ((midics (lmidic value)))
       (get-staff-from-midics midics)))

   ((equal class-of-value 'voice)
     (let ((midics (mapcar #'lmidic (chords value))))
      (get-staff-from-midics midics)))

  (t (progn (om-message-dialog "?") (om-abort)))))) 
            
(defun get-staff-from-midics (midics)
 (cond 
  ((numberp midics)
     (cond ((< midics 3600) 'ff)
                ((< midics 6000) 'f)
                ((<= midics 8400) 'g)
                (t 'gg)))
 (t (let ((midic-min (list-min (flat midics)))
           (midic-max (list-max (flat midics))))
     (cond 
      ((< midic-min 3600)
       (cond ((< midic-max 6400) 'ff)
                 ((<= midic-max 8400) 'gff)
                 (t 'ggff)))

     ((< midic-min 5500)
      (cond ((<= midic-max 6400) 'f)
                ((<= midic-max 8400) 'gf)
                (t 'ggf)))

   (t (if (<= midic-max 8400) 'g 'gg)))))))

