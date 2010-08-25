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

(defmethod get-ray-behind-objective ((obj objective)
				     x-mm y-mm bfp-ratio-x bfp-ratio-y)
  "Take a point on the back focal plane and a point in the sample and
 calculate the ray direction ro that leaves the objective. So its the
 same calculation that is used for draw-ray-into-vol."
  (declare (double-float x-mm y-mm bfp-ratio-x bfp-ratio-y)
	   (values ray &optional))
  (with-slots ((bfp bfp-radius)
	       (f focal-length)) obj
    (let* ((theta (find-inverse-ray-angle obj x-mm y-mm))
	   (phi (atan y-mm x-mm)))
      (refract (make-instance 'ray 
			      :start (make-vec (* bfp-ratio-x bfp)
					       (* bfp-ratio-y bfp)
					       (- f))
			      :direction (v-spherical theta phi))
	       obj))))

#+nil
(get-ray-behind-objective .1d0 .1d0 0d0 0d0)
