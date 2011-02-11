(eval-when (:compile-toplevel :execute :load-toplevel)
  (require :gui) ;; make sure DISPLAY is exported
  (require :clara)
  (require :mma)
  (require :focus)
  (setf asdf:*central-registry*
	'("../../0102/woropt-cyb-0628/"
	  "../../0131/rayt/"
	  "../../0126/bead-eval/"))
 (require :rayt)
 (require :vol)
 (require :bead-eval))


#+nil ;; for saveing lisp datastructures
(require :cl-store)
#+nil ;; save data
(progn
 (cl-store:store *stack* (dir "stack.store"))
 nil)
#+nil ;; load data again
(defparameter *stack* (cl-store:restore (dir "auswert/stack.store")))


(defun extract (a &key (start nil) (center nil) (size nil))
  (let ((b (make-array size :element-type (array-element-type a))))
    (destructuring-bind (w h) (array-dimensions a)
     (destructuring-bind (yy xx) size
       (let* ((centert (if center
			   center
			   (mapcar #'(lambda (x) (floor x 2))
				   (list h w)))) 
	      (startt (if start
			  start
			  (mapcar #'- centert
				  (mapcar #'(lambda (x) (floor x 2))
					  size))))
	      (a1 (sb-ext:array-storage-vector a)))
	 (destructuring-bind (sy sx) startt
	   (vol:do-region ((j i) (yy xx))
	     (setf (aref b j i) (aref a1 (+ (+ sx i) (* w (+ sy j))))))
	   b))))))
;;
;(load "../../0126/bead-eval/bead-eval.lisp")

;; set this to y x of the upper left edge of the region of interest
(defparameter *extract-offset* '(264 420))

(defun extract-stack (stack)
  (let* ((h 600)
	(w 700)
	(n (list-length stack))
	(vol (make-array (list n h w) :element-type 'single-float)))
    (dotimes (k n)
      (destructuring-bind (z img) (elt stack k)
	(declare (ignore z))
	(let ((slice (vol:convert-2-ub16/sf-mul 
		      (bead-eval:byte-swap (extract img
						    :start *extract-offset*
						    :size (list h w))))))
	  (vol:do-region ((j i) (h w))
	    (setf (aref vol k j i) (aref slice j i))))))
    (the (simple-array single-float 3) vol)))

#+nil
(time (defparameter *p*
   (extract-stack *stack*)))
#+nil
(time
 (defparameter *g3* (bead-eval:make-gauss3 *p* :sigma-x-pixel 5s0)))
#+nil
(time
 (defparameter *bp* (vol:convert-3-csf/sf-realpart 
		     (vol:convolve-circ-3-csf *g3*
					      (vol:convert-3-sf/csf-mul *p*)))))
#+nil
(vol:save-stack-ub8 (dir "filtered-stack") (vol:normalize-3-sf/ub8 *bp*))

#+nil
(vol:save-stack-ub8 (dir "seeds")
		(vol:normalize-3-sf/ub8
		 (run-ics::mark-nuclear-seeds *bp* :threshold .1)))
#+nil ;; select the biggest 11 maxima in 27 neighbourhood, these are the beads
(subseq (run-ics::point-list-sort (run-ics::nuclear-seeds *bp*))
	0 5)

#+nil ;; determine the bead positions in the camera coordinate system
(let ((l (destructuring-bind (yo xo) *extract-offset*
	   (mapcar #'(lambda (l) (destructuring-bind (h (z y x)) l
			     (declare (ignore h))
			     (list z y x)))
		  (subseq (run-ics::point-list-sort 
			   (run-ics::nuclear-seeds *bp*))
			  0 4)))))
  (setf rayt-model:*centers-fix*
	(make-array (length l)
		    :element-type 'rayt-model::vec
		    :initial-contents
		    (mapcar #'(lambda (q)
				(destructuring-bind (z y x) q
				  (declare (type fixnum z y x))
				  (v z y x)))
			    l)))
  (setf rayt-model:*data-dz-mm* 2s-3)
 (setf rayt-model:*data-dx-mm* .1s-3)
 ;; assume that the center of the camera lies on the optical axis
 (destructuring-bind (z y x) (array-dimensions *p*)
  (defparameter *data-center-x-px* (* .5 x))
     (defparameter *data-center-y-px* (* .5 y)) 
   (setf rayt-model:*data-width-px* x))
 ;; coordinates of the nuclei in mm, relative to center of camera and first slice
 (setf rayt-model:*centers*
       (make-array 
	(length l)
	:element-type 'rayt::vec
	:initial-contents
	(mapcar #'(lambda (q)
		    (destructuring-bind (z y x) q
		      (declare (type fixnum z y x))
		      (v       (* rayt-model:*data-dz-mm* z) 
			       (* rayt-model:*data-dx-mm* 
				  (- y *data-center-y-px*))
			       (* rayt-model:*data-dx-mm* 
				  (- x *data-center-x-px*))
			       )))
		l))))
#+nil
(loop for k below 20 collect
 (rayt-model::get-nuclei-in-slice k 2))

#+nil ;; draw nuclei into each slice
(loop for i below 20 do
     (let ((img (make-array (list 100 100) :element-type '(unsigned-byte 8))))
       (dolist (e (rayt-model::get-nuclei-in-slice i 2))
	 (rayt-model::draw-nucleus img e))
       (write-pgm (dir "lcos-~3,'0d.pgm" i) img)))


#+nil ;; illuminate center of one nucleus NUC protect others
(dotimes (nuc (length rayt-model:*centers*))
 (defparameter *bfp*
   (let ((bfp (rayt::make-image 256)))
     (dotimes (protect 11)
       (simple-rayt:project-nucleus-into-bfp 
	bfp nuc protect
	(let ((e (aref rayt-model:*centers* nuc)))
	  (v 0 (vy e) (vz e))) ; careful
	:radius-mm 2s-3))
     (write-pgm (dir "bfp-00-~3,'0d.pgm" nuc)
		(vol:normalize-2-ub8/ub8 bfp))
     bfp)))

#+nil ;; illuminate several points inside ffp
(time
 (dotimes (nuc (length rayt-model:*centers*))
   (defparameter *bfp-sum*
     (let ((bfp (rayt::make-image 256)))
       (dotimes (protect 11)
	 (multiple-value-bind (bfp2 ffp)
	     (simple-rayt:sum-bfp-raster
	      bfp nuc protect
	      :radius-ffp-mm 3s-3
	      :radius-project-mm 2s-3
	      :w-ffp 256)
	   (when (= protect nuc) ;; store LCoS image
	     (write-pgm (dir "ffp-~3,'0d.pgm" nuc)
			(vol:normalize-2-ub8/ub8 ffp)))))
       (write-pgm (dir "bfp-sum-00-~3,'0d.pgm" nuc)
		  (vol:normalize-2-ub8/ub8 bfp))
       bfp))))

#+nil ;; EXPORT a 3D model
(with-open-file (s (dir "model.asy") :direction :output
		   :if-exists :supersede
		   :if-does-not-exist :create)
  (macrolet ((asy (str &rest rest)
	       `(progn
		  (format s ,str ,@rest)
		  (terpri s))))
    (flet ((coord (v)
	     (format nil "(~f,~f,~f)"
		     (vz v) (vy v) (vx v)))) ; careful
      (let* ((f (rayt-model:find-focal-length 63s0))
	     (na 1.4s0)
	     (ri 1.515s0)
	     (rif (* ri f))
	     (r (rayt::find-bfp-radius na f)))
	(asy "import three;~%import grid3;")
	(asy "size(300,300);")
	(dotimes (i (length rayt-model:*centers*))
	  (let ((pos (.s ri (aref rayt-model:*centers* i)))
		(rad (* ri 1.5s-3)))
	   (asy "draw(shift(~a)*scale3(~f)*unitsphere,~a);"
		(coord pos) rad
		(if (= i 0)
		    "red+opacity(0.4)"
		    "lightgreen+opacity(0.2)"))
	   (asy "draw(~a--~a);" ;; line from bottom of nucleus to 0
		(coord (rayt::.- pos (v rad)))
		(coord (v 0 (vy pos) (vz pos)))) ; careful
	   (asy "label(~s,~a);" (format nil "~d" i) (coord pos))))
	;; draw 20um segment in z-direction
	(asy "draw(~a--~a);" 
	     (coord (.s ri (aref rayt-model:*centers* 0)))
	     (coord (rayt::.+ (v .01 0 0)
			      (.s ri (aref rayt-model:*centers* 0)))))
	#+nil (progn ;; draw rest of objective
	  ;; bfp
	 (asy "draw(shift(~f,~f,~f)*scale3(~f)*unitsquare3);"
	      (- r) (- r)
	      (* -1 (+ rif f))
	      (* 2 r))
	 ;; field
	 (let ((field .1))
	   (asy "draw(shift(~f,~f,0)*scale3(~f)*unitsquare3);"
		(- field) (- field)
		(* 2 field)))
	 ;; lens
	 (asy "draw(shift(0,0,~f)*scale3(~f)*unitcircle3);"
	      (- rif)
	      r))
       (asy "grid3(XYZgrid);")))))


;; for homology

(deftype num () `single-float)
(deftype vec () `(simple-array num (3)))
(deftype mat () `(simple-array num (3 3)))

(declaim (inline vx vy vz))
(defun vx (v) (declare (type vec v)) (the num (aref v 0)))
(defun vy (v) (declare (type vec v)) (the num (aref v 1)))
(defun vz (v) (declare (type vec v)) (the num (aref v 2)))

(defun v (&optional (x 0s0) (y 0s0) (z 0s0))
  (the vec (make-array 3 :element-type 'num :initial-contents (list (float x) 
								    (float y)
								    (float z)))))

#+nil
(vx (v 1 2 34))

(defun m (a b c d e f g h i)
  (declare (type num a b c d e f g h i))
  (the mat 
    (make-array '(3 3)
		:element-type 'num
		:initial-contents (list (list a b c)
					(list d e f)
					(list g h i)))))

(defun m* (matrix vect)
  "Multiply MATRIX with VECT."
  (declare (type mat matrix) (type vec vect))
  (let ((res (v)))
    (dotimes (i 3)
      (dotimes (j 3)
        (incf (aref res i)
              (* (aref matrix i j) (aref vect j)))))
    res))


(defun .s (s a)
  "scalar multiplication"
  (declare (type num s)
	   (type vec a))
  (let ((r (v)))
    (dotimes (i (length a))
      (setf (aref r i) (* s (aref a i))))
    r))

(defun cam->lcos (cam)
  (declare (type vec cam))
  (let* ((slcos (m* (m -1.0437 0.02797 1339.98
		       .001000 1.05462 -6.178
		       2.64e-6 2.48e-5 .986)
		    cam)))
    (the vec (.s (/ (vz slcos)) slcos))))

#+nil
(cam->lcos (v 0 0 1))


#+nil ;; FILL two screens
(sb-thread:make-thread 
 #'(lambda ()
     (sb-ext:run-program "/usr/bin/xset"
			 '("-dpms" "s" "off"))
     (gui:with-gui #+nil ((+ 1280 1366) 1024 -1 -1)
		   (1280 1024 (+ 1366 -1) -1)
       (draw-screen)))
 :name "display")




(defparameter *exposure-time-s* .0163s0)
;;; Clara CAMERA
#+nil
(progn
  (setf *exposure-time-s* (* .0163s0 4))
 (clara:init :exposure-s  *exposure-time-s*
	     :fast-adc t
	     :external-trigger t
	     ;:xpos -290 :ypos 100
	     ;:width 128 :height 128
	     :width 1392 :height 1040
	     ))
;; continuously capture new images
(defparameter *capture-thread* nil)
(defun start-capture-thread (&optional (delay nil))
  (unless *capture-thread*
   (setf *capture-thread*
	 (sb-thread:make-thread
	  #'(lambda ()
	      (loop
		 (when delay
		  (sleep delay))
		 (clara:snap-single-image)))
	  :name "capture"))))

(defun stop-capture-thread ()
  (let ((is-running *capture-thread*))
   (when is-running
     (sb-thread:terminate-thread *capture-thread*)
     (setf *capture-thread* nil))
   is-running))

(defun set-manual ()
  (stop-capture-thread)
  (mma::set-stop-mma)
  (setf *bright-im* t)
  (setf *exposure-time-s* (* .0163s0 84))
  (clara:init :exposure-s  *exposure-time-s*
	      :fast-adc t
	      :external-trigger t
	      :width 32 :height 32)
  (start-capture-thread 0s0))

(defun set-automatic ()
  (stop-capture-thread)
  (setf *exposure-time-s* (* .0163s0 4))
  (clara:init :exposure-s  *exposure-time-s*
	      :fast-adc t
	      :external-trigger t
	      :width 1392 :height 1040
	      )
  (mma::set-start-mma)
  (setf *bright-im* nil))

#+nil
(set-manual)
#+nil
(set-automatic)
#+nil
(start-capture-thread 0s0)
#+nil
(stop-capture-thread)
#+nil
(clara:stop)
#+nil
(clara:status)
#+nil
(clara:snap-single-image)
#+nil
(clara:uninit)
#+nil
clara:*im*

;;; MMA OVER NETWORK (run sudo ifconfig eth1 192.168.0.1)
;; oscilloscope luvcview -d /dev/video1 -s 320x240 -i 5
;; mma image    luvcview -d /dev/video0 -s 320x240 -i 5
;; /usr/local/bin/uvcdynctrl -v -s "Exposure (Absolute)" 10000

#+nil
(progn
 (mma:init)
 (mma:set-nominal-deflection-nm 118.25))
#+nil
(mma:set-nominal-deflection-nm 118.25)
#+nil
(mma:status)
#+nil
(mma::set-power-off)
#+nil
(mma:get-nominal-deflection-nm)
#+nil
(mma::set-start-mma)
#+nil
(mma::set-stop-mma)
#+nil
(progn
  (mma::set-stop-mma)
  (mma::set-extern-trigger t)
  (let* ((e-ms (* 1000 *exposure-time-s*))
	(e-us (* 1000 e-ms)))
    (mma::set-deflection-phase 0s0 e-us)
    (mma::set-cycle-time (* 2 e-ms)))
  (mma:begin))
#+nil
(progn
  (mma::set-stop-mma)
  (mma::set-extern-trigger nil)
  (let* ((e-ms (* 1000 *exposure-time-s*))
	(e-us (* 1000 e-ms)))
    (mma::set-deflection-phase 0s0 e-us)
    (mma::set-cycle-time (* 8 e-ms)))
  (mma:begin))
#+nil
(mma::get-cycle-time)
#+nil

(defvar *mma-contents* nil)
(defvar *mma-select* 0)

(defparameter *mma-dark* 26)
(defparameter *mma-bright* 25)
#+nil
(mma::select-pictures 0 :n 9 :ready-out-needed t)
#+nil
(select-disk *mma-bright*)
#+nil
(dotimes (j 12)
 (let ((n 30)
       (dr/2 .025s0))
   (dotimes (i n) 
     (let ((r (+ dr/2 (* (- 1s0 (* 2 dr/2))
			 (/ (* 1s0 i) n)))))
       (mma::draw-disk-cal :r-small (- r dr/2) :r-big (+ r dr/2) :pic-number 5))
     #+nil (sleep .1))))

#+nil ;; draw some ARBITRARY data
(let* ((n 256)
       (m (make-array (list n n) :element-type '(unsigned-byte 12))))
  (dotimes (j n)
    (dotimes (i n)
      (setf (aref m j i) (* #+nil (* (mod i 2) (mod j 2)) 4095))))
  (mma:draw-array-cal m :pic-number 5)
  nil)
#+nil
(select-disk 4)

(defvar *mma-contents* nil)
#+nil
(time
 (progn
   (mma::end)
   #+nil (dotimes (i 4090)
	   (mma::draw-random-cal :pic-number (1+ i)))
   #+nil (mma:load-white :radius 1.0 :pic-number 1)
   #+nil (mma:load-concentric-circles :n 12)
   #+nil (setf *mma-contents* (mma::load-concentric-disks :n 12))
   #+nil (setf *mma-contents* (mma::draw-ring-cal :r-small .3 :r-big .8))
   #+nil (setf *mma-contents* (mma::load-concentric-circles :dr .1 :n 12))
   (let ((n 3))
     (setf *mma-contents* (mma:load-disks2 :n n)
	   *mma-bright* (* n n)
	   *mma-dark* (+ (* n n) 1))
     (append *mma-contents* 
	     (list (mma::draw-disk-cal :pic-number (1+ *mma-bright*))
		   (mma::draw-disk-cal :pic-number (1+ *mma-dark*) 
				       :value 0))))
   #+nil(mma::draw-grating)
   (mma:begin)))
#+nil
(mma:uninit)
#+nil
(mma::reset)




;;; FOCUS STAGE OVER SERIAL (look for pl2303 converter in dmesg)
#+nil
(focus:connect)
#+nil
(focus:get-position)
#+nil ;; move away from sample
(focus:set-position (- (focus:get-position) .25))
#+nil ;; move further into sample
(focus:set-position (+ .25 (focus:get-position)))
#+nil
(focus:disconnect)


(defvar *section-im* nil)
(defvar *widefield-im* nil)
(defvar *unfocused-im* nil)
(defvar *dark-im* nil)
(defvar *bright-im* nil)
(defvar *stack* nil)

#+nil ;; show disk
(setf *bright-im* t)
#+nil ;; show grating
(setf *dark-im* t)

(defparameter *data-dir* "/home/martin/d0211/")

(defparameter *illum-target* (v))

(defmacro dir (str &rest rest)
  `(concatenate 'string *data-dir* (format nil ,str ,@rest)))

(defun  obtain-image (&optional (mma-image *mma-bright*))
  (select-disk mma-image)
  (setf *dark-im* nil)
  (setf *bright-im* t)
  (sleep .1)
  (clara:snap-single-image)
  (write-pgm16 (dir "snap~3,'0d.pgm" mma-image) 
	       clara:*im*)
  (setf *bright-im* nil
	*dark-im* nil)
  (select-disk *mma-bright*))

(defun clamp-u16 (a)
  (declare (values (unsigned-byte 16) &optional))
  (let ((ma #.(1- (expt 2 16))))
   (cond ((< a 0) 0)
	 ((< ma a) ma)
	 (t a))))

;; capture image with angular illumination
#+nil
(progn
  (setf *bright-im* t
	*dark-im* nil)
  (sleep .1)
  (dotimes (i *mma-dark*)
   (obtain-image i)
   (sleep 1))
 (setf *bright-im* nil
       *dark-im* t)
 (obtain-image *mma-dark*))

;; store MMA images
#+nil
(let ((i 0))
 (dolist (e *mma-contents*)
   (let ((r (make-array (butlast (array-dimensions e))
			:element-type '(unsigned-byte 8))))
     (destructuring-bind (y x) (array-dimensions r)
       (dotimes  (j y)
	 (dotimes (i x)
	   (setf (aref r j i) (aref e j i 0)))))
     (write-pgm (dir "mma~3,'0d.pgm" i) r))
   (incf i)))

#+nil
(obtain-image)

(defun select-illumination-target (nuc)
  (let* ((b (aref rayt-model:*centers-fix* nuc))
	 (z-stage-um (first (elt *stack* (floor (vx b)))))     ; careful
	 (x-cam-px (+ (second *extract-offset*) (vz b)))	     ; careful
	 (y-cam-px (+ (second *extract-offset*) (vy b))))
    (setf *illum-target* (cam->lcos (v x-cam-px y-cam-px 1)))
    (setf (aref *illum-target* 2) z-stage-um)))

#+nil
(select-illumination-target)

(defun capture-nucleus (nuc)
 #+nil (let ((start-z (focus:get-position)))
    (focus:set-position (vz *illum-target*))
    ;; do things here
    (focus:set-position start-z))
 (setf *dark-im* t) ;; don't show grating
 (setf *bright-im* t)
 (select-illumination-target nuc)
 (sleep .1)
 (clara:snap-single-image)
 (write-pgm16 (dir "snap~3,'0d.pgm" nuc) 
	      clara:*im*)
 (setf *bright-im* nil
       *dark-im* t))

#+nil
(dolist (e '(3 7 0 5))
 (capture-nucleus e))

;; OBTAIN
#+nil
(obtain-sectioned-slice)
#+nil
(progn
 (obtain-stack :n 18 :offset 3 :dz 2s0)
 nil)
(declaim (optimize (speed 1) (debug 3) (safety 3)))
;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 2)
       (phases-x 4)
       (phases-y 1)
       (a (make-array (* phases-x phases-y white-width) 
		      :element-type '(unsigned-byte 8)))
       (phase 0)
       
       ;(im-scale 100s0) (im-offset 1.86s0)
       (im-scale 20s0) (im-offset 0.019s0)
       )

  (defun change-phase (p)
    (setf phase p
	  a (gui:grating->texture (gui:grating-stack phases-y phases-x)
				  p :h white-width :w white-width))) 

  (change-phase 0)

  (defun draw-disk-fan (&key (radius 1.0) (n 17))
    (gl:with-pushed-matrix
      (gl:scale radius radius radius)
      (gl:with-primitive :triangle-fan
	(gl:vertex 0.0 0.0)
	(dotimes (i n)
	  (let ((phi (* i (/ (* 2.0 (coerce pi 'single-float)) n))))
	    (gl:vertex (cos phi) (sin phi))))
	(gl:vertex 1.0 0.0))))

  (defun obtain-sectioned-slice (&key (mma-image *mma-bright*)
				 (accumulate 1) (z 0))
    (let ((capture-was-running (stop-capture-thread)))
      (destructuring-bind (w h) (array-dimensions clara:*im*)
	
	(let ((dark-im (make-array (list w h)
				   :element-type '(unsigned-byte 16))))
	  ;; capture a dark image
	  (setf *dark-im* t) ;; dark LCOS
	  (select-disk *mma-dark*)
	  (sleep 1/60)
	  (clara:snap-single-image)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref dark-im i j) (aref clara:*im* i j))))
	  (write-pgm16 (dir "dark-~3,'0d.pgm" z)
		       dark-im)
	  ;; capture bright widefield image
	  (select-disk *mma-bright*)
	  (setf *dark-im* nil)
	  (setf *bright-im* t)
	  (sleep 1/60)
	  (clara:snap-single-image)
	  (dotimes (j h)
	    (dotimes (i w)
	      (setf (aref dark-im i j) (aref clara:*im* i j))))
	  (write-pgm16 (dir "bright-~3,'0d.pgm" z)
		       dark-im)
	  (setf *bright-im* nil
		*dark-im* nil)
	  (select-disk mma-image)
	  (sleep 1/60))
	(format t "dark and bright image captured~%")
	(let ((phase-im (make-array (list phases-y phases-x w h) 
				    :element-type '(signed-byte 64)))
	      (widefield-im (make-array (list w h) 
					:element-type '(signed-byte 64))))
	  (setf *section-im* (make-array (list w h) 
					 :element-type '(unsigned-byte 16))
		*widefield-im* (make-array (list w h) 
					   :element-type '(unsigned-byte 16)))
	  ;; capture the phase images, accumulate if requested, update
	  ;; widefield image as well
	 (dotimes (py phases-y)
	   (dotimes (px phases-x)
	     (change-phase (+ px (* phases-x py)))
	     (format t "~a~%" (list 'z z "phase" 'px px 'py py))
	     (sleep .5)
	     (clara:snap-single-image)
	     (dotimes (a accumulate)
	       (dotimes (j h)
		 (dotimes (i w)
		   (let ((v (aref clara:*im* i j)))
		     (incf (aref phase-im py px i j) v)
		     (incf (aref widefield-im i j) v)
		     (setf (aref *widefield-im* i j)
			   (clamp-u16 (floor (aref widefield-im i j) 2)))))))
	     (write-pgm16 (dir "phase-~3,'0d-~3,'0d-~3,'0d.pgm"
			       z py px)
			  clara:*im*)))
	 ;; final widefield image normalize to full 16bit range
	 (let ((ma 0)
	       (mi (1- (expt 2 64))))
	   (dotimes (j h)
	     (dotimes (i w)
	       (let ((v (aref widefield-im i j)))
		 (setf ma (max v ma)
		       mi (min v mi)))))
	   (dotimes (j h)
	     (dotimes (i w)
	       (setf (aref *widefield-im* i j) 
		     (floor (- (aref widefield-im i j) mi)
			    (/ (- ma mi) (1- (expt 2 16)))))))
	   (format t "values in accumulated widefield image: ~a~%" (list mi ma))
	   ;; min-max reconstruction
	   (progn
	    (dotimes (j h)
	      (dotimes (i w)
		(let* ((v (aref phase-im 0 0 i j))
		       (ma v)
		       (mi v))
		  (dotimes (py phases-y)
		    (dotimes (px phases-x)
		      (let ((v (aref phase-im py px i j)))
			(setf mi (min mi v)
			      ma (max ma v)))))
		  (setf (aref *section-im* i j) (clamp-u16 (- ma mi))))))
	    (write-pgm16 (dir "section-~3,'0d.pgm" z)
	     *section-im*))
	   
	   ;; subtract section image from accumulated widefield image
	   (setf *unfocused-im* (make-array
				 (list w h)
				 :element-type '(unsigned-byte 16)))
	   (dotimes (j h)
	     (dotimes (i w)
	       (setf (aref *unfocused-im* i j)
		     (clamp-u16 
		      (- (aref *widefield-im* i j) 
			 (floor (aref *section-im* i j) 
				;; uses max and min from widefield
				(/ (- ma mi) (* 20 (1- (expt 2 16))))))))))))
	(format t "maximum value in section ~a~%"
		(reduce #'max (sb-ext:array-storage-vector *section-im*))))
      (when capture-was-running
	(start-capture-thread))))
  
  (defun obtain-stack (&key (n 30) (dz .1s0) (offset (floor n 2)) (accumulate 1))
    (declare (ignore accumulate))
    (let ((center (focus:get-position))
	  (result nil))
      (dotimes (k n)
	(let ((z (+ center (* dz (- k offset)))))
	  (format t "slice ~f ~d/~d~%" z (1+ k) n)
	  (focus:set-position z)
	  (sleep .1)
	  (obtain-sectioned-slice :z k)
	  (push (list z *section-im*) result)))
      (focus:set-position center)
      (setf *stack* (reverse result))))

  (defun draw-screen ()
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit)
    ;; BACK draw raw camera image on the left
    (gl:with-pushed-matrix
      
      (gl:scale .25 .25 1s0)
      (gl:translate 0 900 0)
      #+nil (when clara:*im*
	(let ((tex (make-instance 'gui::texture :data clara:*im* 
				  :scale im-scale :offset im-offset)))
	  (destructuring-bind (w h) (array-dimensions clara:*im*)
	    (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	  (gui:destroy tex)))
      ;; draw SECTIONed image next to it
      #+nil(gl:with-pushed-matrix
	(when *section-im*
	  (let ((tex (make-instance 'gui::texture :data *section-im*
				    :scale 3000s0 :offset 0.0005s0)))
	    (destructuring-bind (w h) (array-dimensions *section-im*)
	      (gl:translate w 0 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex)))
	;; draw accumulated widefield image below
	#+nil(when *widefield-im*
	  (let ((tex (make-instance 'gui::texture :data *widefield-im*
				    :scale 7s0 :offset 0.3s0
				    )))
	    (destructuring-bind (w h) (array-dimensions *widefield-im*)
	      (gl:translate 0 h 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex))))
      ;; draw an image with only out of focus light in the lower left
      #+nil(gl:with-pushed-matrix
	(when *unfocused-im*
	  (let ((tex (make-instance 'gui::texture :data *unfocused-im*
				    :scale 10s0 :offset 0.0s0)))
	    (destructuring-bind (w h) (array-dimensions *section-im*)
	      (gl:translate 0 h 0)
	      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
	    (gui:destroy tex))))
      ;; draw the image that is displayed on the mma
      #+nil (when *mma-contents*
	      (gl:with-pushed-matrix
		(let ((p (elt *mma-contents* *mma-select*))) 
		  (let ((tex (make-instance 'gui::texture :data p
					    :scale 1s0 :offset 0.0s0)))
		    (destructuring-bind (w h) (array-dimensions p)
		      (gl:translate 0 (* 2 h) 0)
		      (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)))
		    (gui:destroy tex)))))
      )    
    ;; draw grating for sectioning on the very right
    (gl:with-pushed-matrix 
      ;(gl:translate (+ 280 400) 0 0)
      (unless *dark-im*
	(let ((repetition 900f0))
	  (gui::with-grating (g a)
	    (gui:draw g 
		      :w (* repetition white-width phases-x)
		      :h (* repetition white-width phases-y)
		      :wt repetition
		      :ht repetition)))))

    ;; FAN
    (when *bright-im*
     (gl:with-pushed-matrix
       ;(gl:translate 1366 0 0)
       (gl:color 1 1 1)
       (gl:translate (vx *illum-target*) (vy *illum-target*) 0.0)
       (draw-disk-fan :radius 1000.0)))

    #+nil
    (gl:with-primitive :lines
      (gl:color 1 0 0) (gl:vertex 0 0) (gl:vertex (* white-width phases 3) 0 0) ;; x axis red
      ;; show ruling with a pixel spacing of white-width
      (dotimes (i (1+ (* phases 3)))
	(let ((x (* white-width i))
	      (y (if (/= 0 (mod i phases)) -3 -5)))
	  (gl:vertex x 0)
	  (gl:vertex x y)))
      (gl:color 0 1 0) (gl:vertex 0 1) (gl:vertex 0 100 0) ;; y axis green
      (gl:color 0 0 1) (gl:vertex 0 0 1) (gl:vertex 0 0 100))))

(defun select-disk (q)
  (setf *mma-select* q)
  (mma:select-pictures q :n 1 :ready-out-needed t))

#+nil
(select-disk 4)

#+nil
(sb-thread:make-thread 
 #'(lambda ()
     (loop
	(dotimes (i 26)
	(select-disk i)
	(sleep .5))))
 :name "switch-angle")

(defun average-img (img)
  (let ((sum 0))
   (destructuring-bind (w h) (array-dimensions img)
     (dotimes (j h)
       (dotimes (i w)
	 (incf sum (aref img i j))))
     (/ sum (* 1s0 w h)))))

(defun integrate-slices (stack)
  (loop for s in stack collect
    (destructuring-bind (z slice) s
      (list z
	    (average-img slice)))))

(defparameter *stack-integral* nil)


(defun write-pgm (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 8) 2) img)
           (values null &optional))
  (destructuring-bind (h w)
      (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%255~%" h w))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 8)
                       :direction :output
                       :if-exists :append)
      (write-sequence (sb-ext:array-storage-vector img) s))
    nil))

(defun write-pgm16 (filename img)
  (declare (simple-string filename)
           ((array (unsigned-byte 16) 2) img)
           (values null &optional))
  (destructuring-bind (h w) (array-dimensions img)
    (declare ((integer 0 65535) w h))
    (with-open-file (s filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (declare (stream s))
      (format s "P5~%~D ~D~%65535~%" h w))
    (with-open-file (s filename 
                       :element-type '(unsigned-byte 16)
                       :direction :output
                       :if-exists :append)
      (write-sequence (sb-ext:array-storage-vector img) s))
    nil))

;; save the bfp scan as a mosaic
(defmacro save-bfp-mosaic (filename image)
  `(destructuring-bind (w h) (array-dimensions (third (elt *mma-scan* 0)))
    (let* ((n 7) 
	  (mosaic (make-array (list (* w n) (* h n)) :element-type '(unsigned-byte 8))))
      (dolist (e *mma-scan*)
	(destructuring-bind (ii jj wide section unfocus) e
	  (let ((ma (reduce #'max (sb-ext:array-storage-vector wide)))
		(mi (reduce #'min (sb-ext:array-storage-vector wide))))
	    (dotimes (j h)
	      (dotimes (i w)
		(setf (aref mosaic (+ (* ii w) i) (+ (* jj h) j))
		      (floor (- (aref ,image i j) mi)
			     (/ (- ma mi) 255))))))))
     (write-pgm (format nil "~a-~a.pgm" ,filename ',image) mosaic))))
#+nil
(progn
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" wide)
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" section)
 (save-bfp-mosaic "/home/martin/tmp/mosaic7" unfocus))
