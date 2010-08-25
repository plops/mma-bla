(in-package :lens)

;; 		        |
;; --------		|
;;   SS    \---		|
;;             \--      |
;;                X-----+-------------------------------
;;              /-  \   |
;;            /-     \  |
;;    nf=h  /-        \ | D/2=R
;;        /-           \|
;;      /-              |
;;    /- alpha	        |
;; ---------------------+----------------------------------------
;;             nf       |
;;
;; sin(alpha) = R/nf

(defmethod back-focal-plane-radius ((objective objective))
  (declare (values double-float &optional))
  (with-slots (focal-length numerical-aperture) objective
    (* focal-length numerical-aperture)))

(defun focal-length-from-magnification (mag)
  (declare (double-float mag)
           (values double-float &optional))
  (/ 164.5 mag))
#+nil
(focal-length-from-magnification 63d0)

(defun make-objective (&key (magnification 63d0)
                       (numerical-aperture 1.38d0)
                       (immersion-index 1.515d0)
                       (center (v))
                       (normal (v 0 0 1)))
  (let* ((f (focal-length-from-magnification magnification))
	 (o (make-instance 'objective 
                           :bfp-radius 1d0 ;; these need to be generated
                           :focal-length f
                           :radius 1d0
                           :numerical-aperture numerical-aperture
                           :immersion-index immersion-index
                           :center center
                           :normal normal))
         (bfp-radius (back-focal-plane-radius o)))
    (make-instance 'objective 
                   :bfp-radius bfp-radius
                   :focal-length f
                   :radius (* 10 bfp-radius)
                   :numerical-aperture numerical-aperture
                   :immersion-index immersion-index
                   :center center
                   :normal normal)))
#+nil
(make-objective)
