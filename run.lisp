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

(defparameter *data-dir* "/home/martin/d0214/")
(defmacro dir (str &rest rest)
  `(concatenate 'string *data-dir* (format nil ,str ,@rest)))

(defvar *section-im* nil)
(defvar *widefield-im* nil)
(defvar *unfocused-im* nil)
(defvar *dark-im* t)
(defvar *bright-im* nil)
(defvar *stack* nil)

(defun show-disk (&optional (on t))
 (setf *bright-im* on))

(defun show-grating (&optional (on t))
 (setf *dark-im* (not on)))


#+nil ;; for saveing lisp datastructures
(require :cl-store)
#+nil ;; save data
(progn
 (cl-store:store *stack* (dir "stack.store"))
 nil)
#+nil ;; load data again
(defparameter *stack* (cl-store:restore "/data/dat/d0210/auswert/stack.store"))


(defun extract (a &key (start nil) (center nil) (size nil))
  (let ((b (make-array size :element-type (array-element-type a))))
    (destructuring-bind (h w) (array-dimensions a)
     (destructuring-bind (yy xx) size
       (let* ((centert (if center
			   center
			   (mapcar #'(lambda (x) (floor x 2))
				   (list h w)))) 
	      (startt (if start
			  start
			  (mapcar #'- centert
				  (mapcar #'(lambda (x) (floor x 2))
					  size)))))
	 (destructuring-bind (sy sx) startt
	   (vol:do-region ((j i) (yy xx))
	     (setf (aref b j i) (aref a (+ sy j) (+ sx i))))
	   b))))))

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
		      (extract img
				:start *extract-offset*
				:size (list h w))
		      #+nil(bead-eval:byte-swap
		       ))))
	  (vol:do-region ((j i) (h w))
	    (setf (aref vol k j i) (aref slice j i))))))
    (the (simple-array single-float 3) vol)))

(defvar *num-points* 32)
(defvar *p* nil)
(defvar *g3* nil)
(defvar *bp* nil)
(defvar *data-center-x-px* 0) ;; make pixel on optical axis
(defvar *data-center-y-px* 0) ;; the origin
   
(defun find-beads ()
  (setf *p*
	(extract-stack *stack*))
  (setf *g3* (bead-eval:make-gauss3 *p* :sigma-x-pixel 3.1s0))
  (setf *bp* (vol:convert-3-csf/sf-realpart 
		      (vol:convolve-circ-3-csf 
		       *g3* (vol:convert-3-sf/csf-mul *p*))))

  (vol:save-stack-ub8 (dir "seeds")
		      (vol:normalize-3-sf/ub8
		       (run-ics::mark-nuclear-seeds *bp* :threshold .3)))
  
  (let ((l (run-ics::nuclear-seeds *bp*)))
    (multiple-value-bind (hist n mi ma) (run-ics::point-list-histogram l)
      (run-ics::print-histogram hist n (* 1s10 mi) (* 1s10 ma))
      (terpri)
      (setf *num-points* (reduce #'+ (subseq hist 1)))))

  ;; determine the bead positions in the camera coordinate system
  (let ((l (destructuring-bind (yo xo) *extract-offset*
	     (mapcar #'(lambda (l) (destructuring-bind (h (z y x)) l
				(declare (ignore h))
				(list z y x)))
		     (subseq (run-ics::point-list-sort 
			      (run-ics::nuclear-seeds *bp*))
			     0 *num-points*)))))
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
      (setf *data-center-x-px* (* .5 x))
      (setf *data-center-y-px* (* .5 y)) 
      (setf rayt-model:*data-width-px* x))
    ;; coordinates of the nuclei in mm, relative to center of camera
    ;; and first slice
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
  
  ;; EXPORT a 3D model
  (export-3d-model)
  (defparameter *bfps* (prepare-bfps)))
#+nil
(time
 (find-beads))

(defun export-3d-model ()
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
	  (asy "grid3(XYZgrid);"))))))
#+nil
(export-3d-model)


(defparameter *g2*
  
  (bead-eval:make-gauss (make-array 
			 (list 256 256)
			 :element-type '(unsigned-byte 8)) 4s0))

(defun mirror-on-horizontal (img)
  (declare (type (simple-array single-float 2) img))
  (let ((res (make-array (array-dimensions img)
			 :element-type 'single-float)))
    (destructuring-bind (h w) (array-dimensions img)
     (vol:do-region ((j i) (h w))
       (setf (aref res j (- w i 1))
	     (aref img j i)))
     (the (simple-array single-float 2) res))))

(defun select-low-quartile (img &optional (extra-filename "")
			    (illuminating-frac .6s0))
  (declare (type (simple-array (unsigned-byte 64) 2) img))
  (let ((img8 (vol:normalize-2-sf/ub8 (vol::convert-2-ub64/sf-mul img)))
	(hist (make-array 256 :element-type '(unsigned-byte 64))))
    (write-pgm (dir "bfp-img8-~a.pgm" extra-filename) img8)
    (destructuring-bind (h w) (array-dimensions img8)
      (vol:do-region ((j i) (h w)) ;; construct histogram
	(incf (aref hist (aref img8 j i))))
      (let ((maxbin (do ((i 0 (1+ i)) ;; find quantile in cumulative sum
			 (sum 0)) ;; FIXME what about the border?
			((or (< (* illuminating-frac h w) sum)
			     (= i 256))
			 i)
		      (incf sum (aref hist i))))
	    (rad2 (* 128 128)))
	(let ((imgcsf (make-array (list h w) 
				  :element-type '(complex single-float))))
	 (vol:do-region ((j i) (h w))
	   (let* ((x (- i 128))
		  (y (- j 128))
		  (r2 (+ (* x x) (* y y)))
		  (inside (< r2 rad2)))
	    (setf (aref imgcsf j i)
		  (if (and inside (< (aref img8 j i) maxbin))
		      (complex 1s0 0s0)
		      (complex 0s0 0s0)))
	    (setf (aref img8 j i)
		  (if (and inside (< (aref img8 j i) maxbin))
		      255
		      0))))
	 (write-pgm (dir "bfp-bw-~a.pgm" extra-filename) img8)
	 (let ((co (mirror-on-horizontal
		    (vol:convert-2-csf/sf-realpart
		     (vol:convolve-circ-2-csf *g2* imgcsf)))))
	   (write-pgm (dir "bfp-co-~a.pgm" extra-filename)
		      (vol::normalize-2-sf/ub8 co))
	   (vol::normalize-2-sf/ub12 co)))))))


;; illuminate several points inside ffp INVERT
(defun prepare-bfps ()
 (let* ((n  (length rayt-model:*centers*))
	(bfps ()))
   (dotimes (nuc n)
     (defparameter *bfp-sum*
       (let ((bfp (rayt::make-image 256 :type '(unsigned-byte 64))))
	 (dotimes (protect n)
	   (multiple-value-bind (bfp2 ffp)
	       (simple-rayt:sum-bfp-raster
		bfp nuc protect
		:radius-ffp-mm 10s-3
		:radius-project-mm 2s-3
		:w-ffp 200)
	     (declare (ignore bfp2))
	     (when (= protect nuc) ;; store LCoS image
	       (write-pgm (dir "ffp-~3,'0d.pgm" nuc)
			  (vol:normalize-2-sf/ub8 
			   (vol::convert-2-ub64/sf-mul ffp))))))
	 (push (select-low-quartile bfp (format nil "~3,'0d" nuc))
	       bfps))))
   bfps))
#+nil
(defparameter *bfps* (prepare-bfps))



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
  (let* ((slcos (m* (m -1.04631 0.0171617 1346.59
		       .0143984 1.081 -6.64348
		       5.43139e-6 9.59122e-6 .99192)
		    cam)))
    (the vec (.s (/ (vz slcos)) slcos))))

#+nil
(cam->lcos (v 100 100 1))

(defun lcos->cam (lcos)
  (declare (type vec lcos))
  (let* ((scam (m* (m -.949 .00371 1288.3
		      .01295 .94498 -11.2454
		      5.0712e-6 -9.1577e-6 1.0012)
		    lcos)))
    (the vec (.s (/ (vz scam)) scam))))

#+nil
(lcos->cam (v 0 0 1))

(defparameter *window-overlap* 800)

#+nil ;; FILL two screens
(sb-thread:make-thread 
 #'(lambda ()
     (sb-ext:run-program "/usr/bin/xset"
			 '("-dpms" "s" "off"))
     (gui:with-gui ((+ 1280 *window-overlap*) 1024 (+ 1366 
						      (- *window-overlap*) 
						      -1) -1)
       (draw-screen)))
 :name "display")

(defparameter *exposure-time-s* .0163s0)
(progn
  (defvar *scaled-clara-im* nil)
  (defvar *scale-clara-fac* 4))
(declaim (type (unsigned-byte 8) *scale-clara-fac*))

(defun make-scaled-clara-im (&optional (h 1040) (w 1392))
  (setf
   clara:*im* (make-array (list h w)
			  :element-type '(unsigned-byte 16))
   *scaled-clara-im* 
	(make-array 
	 (mapcar #'(lambda (x) (floor x
				 *scale-clara-fac*))
		 (list h w))
	 :element-type '(unsigned-byte 8))))

;;; Clara CAMERA
#+nil
(let ((w 1392) (h 1040))
  (setf *exposure-time-s* (* .0163s0 13))
 (clara:init :exposure-s  *exposure-time-s*
	     :fast-adc t
	     :external-trigger t
	     :width w :height h)
 (make-scaled-clara-im h w)
 nil)
;; continuously capture new images
(defvar *capture-thread* nil)

#+nil ;; INVERT
(time (scale-clara->ub8))

(defun scale-clara->ub8 ()
  (declare (optimize (speed 3))
	   (type (simple-array (unsigned-byte 16) 2) clara:*im*)
	   (type (simple-array (unsigned-byte 8) 2) *scaled-clara-im*))	       
  (destructuring-bind (h w) (array-dimensions *scaled-clara-im*)
    (declare (type (integer 0 10000) w h))
    (let* ((mi (aref clara:*im* 0 0))
	   (ma mi))
      (vol:do-region ((j i) (h w))
	(let* ((v (aref clara:*im*
			(* *scale-clara-fac* j)
			(* *scale-clara-fac* i))))
	  (when (< v mi)
	    (setf mi v))
	  (when (< ma v)
	    (setf ma v))))
      (let ((s (/ 255s0 (- ma mi))))
       (vol:do-region ((j i) (h w))
	 (let* ((v (- (aref clara:*im*
			    (* *scale-clara-fac* j)
			    (* *scale-clara-fac* i)) mi))
		(vs (* v s)))
	   (setf (aref *scaled-clara-im* j i)
		 (truncate vs))))))))


(defun snap-image-and-decimate ()
  (clara:snap-single-image)
  (unless *scaled-clara-im* ;; use every fifth pixel
    (setf *scaled-clara-im* (make-array 
			     (mapcar #'(lambda (x) (floor
					       x
					       *scale-clara-fac*))
				     (array-dimensions clara:*im*))
			     :element-type '(unsigned-byte 8))))
  (scale-clara->ub8))


(defun start-capture-thread (&optional (delay nil))
  (unless *capture-thread*
   (setf *capture-thread*
	 (sb-thread:make-thread
	  #'(lambda ()
	      (loop
		 (when delay
		   (sleep delay))
		 (snap-image-and-decimate)))
	  :name "capture"))))

#+nil
(start-capture-thread)

(defun stop-capture-thread ()
  (let ((is-running *capture-thread*))
   (when is-running
     (sb-thread:terminate-thread *capture-thread*)
     (setf *capture-thread* nil))
   is-running))
#+nil
(stop-capture-thread)

(defun set-manual ()
  (stop-capture-thread)
  (mma::set-stop-mma)
  (setf *bright-im* t)
  (setf *exposure-time-s* (* .0163s0 184))
  (clara:init :exposure-s  *exposure-time-s*
	      :fast-adc t
	      :external-trigger t
	      :width 1392 :height 1040
	      )
  (setf *scaled-clara-im* nil)
  (start-capture-thread 0s0))

(defun set-automatic ()
  (stop-capture-thread)
  (setf *exposure-time-s* (* .0163s0 4))
  (clara:init :exposure-s  *exposure-time-s*
	      :fast-adc t
	      :external-trigger t
	      :width 1392 :height 1040
	      )
  (setf *scaled-clara-im* nil)
  (mma::set-start-mma)
  (setf *bright-im* nil))
#+nil
(set-automatic)
#+nil
(start-capture-thread)
#+nil
(stop-capture-thread)

(defun set-idle ()
  (mma::set-stop-mma))
#+nil
(set-idle)
(defun unset-idle ()
  (mma::set-start-mma))
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
#+nil ;; TRIGGER
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
#+nil ;; draw calibration ring for 10x phase
(mma::draw-disk-cal :r-small .48s0 :r-big .66s0 :pic-number 5)
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


#+nil
(snap-image-and-decimate)

(defparameter *illum-target* (v 532 502))

#+nil
(progn
  (defparameter *illum-target* (v 645 570))
  (snap-image-and-decimate))


(defun  obtain-image (&optional (mma-image *mma-bright*))
  (select-disk mma-image)
  (setf *dark-im* nil)
  (setf *bright-im* t)
  (sleep .1)
  (snap-image-and-decimate)
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
	 (x-cam-px (+ (vz b) (second *extract-offset*) ))	     ; careful
	 (y-cam-px (+ (vy b) (first *extract-offset*) )))
    (setf *illum-target* (cam->lcos (v x-cam-px y-cam-px 1)))
    (setf (aref *illum-target* 2) z-stage-um)
    (format t "~a~%" (list 'cam 'x x-cam-px 'y y-cam-px
			   'target *illum-target*))))
 
#+nil
(select-illumination-target 0)


(defparameter *white-mma*
  (let* ((n 256)
	 (m (make-array (list n n) :element-type '(unsigned-byte 12))))
    (dotimes (j n)
      (dotimes (i n)
	(setf (aref m j i) (* 16 255))))
    m))

#+nil ;; JUMP 
(let* ((do-focus t)
       (do-save t)
       (do-mma t)
       (start-z (when do-focus 
		  (focus:get-position))))
  (unwind-protect
       (progn 
	 (select-disk 4)
	 (setf *bright-im* t)
	 (dotimes (j 10) ;let ((j 5))
	   (format t "~d~%" j)
	   (dotimes (i (length rayt-model:*centers*))
	     (select-illumination-target i)
	     (when do-focus
	       (focus:set-position (aref *illum-target* 2)))
	     (sleep .1)
	     (when do-mma
	       (mma:draw-array-cal (elt *bfps* i) :pic-number 5)
	       (sleep .1)
	       (snap-image-and-decimate)
	       (when do-save
		 (write-pgm16 
		  (dir "snap-angle-illum-~3,'0d-~3,'0d.pgm" j i) 
		  clara:*im*)))
	     
	     (mma:draw-array-cal *white-mma*
				 :pic-number 5)
	     (sleep .1)
	     (snap-image-and-decimate)
	     (when do-save
	       (write-pgm16 (dir "snap-full-illum-~3,'0d-~3,'0d.pgm" j i) 
			    clara:*im*))
	     )))
    (progn
      (setf *bright-im* nil)
      (when do-focus
	(focus:set-position start-z)))))

#+nil
(progn
 (setf *bright-im* t)
 (start-capture-thread))
#+nil
(mma::set-start-mma)
#+nil
(stop-capture-thread)
#+nil ;; position that we originally focussed on
(first (fourth *stack*))
#+nil
(focus:set-position 19.45)

#+nil ;; generate testimage
(progn
 (setf clara:*im* (second (elt *stack* 12)))
 nil)
#+nil
(vol:do-region ((j i) (500 500))
  (setf (aref clara:*im* j i) (* j (floor (1- (expt 2 16)) 500))))
#+nil
(floor (expt 2 16) (reduce #'max (sb-ext:array-storage-vector clara:*im*)))


(defmacro with-focus (z &body body)
  "Store current z-Position go to given Position (in um) and return to
  original position."
  `(let ((start-z (focus:get-position)))
     (unwind-protect 
      (progn 
	(focus:set-position ,z)
	(sleep .1)
	,@body)
      (progn
	(focus:set-position start-z)
	(sleep .1)))))


(defun capture-nucleus (nuc)
  (with-focus (aref *illum-target* 2)
    (setf *dark-im* t) ;; don't show grating
    (setf *bright-im* t) ;; show disk
    (select-illumination-target nuc)
    (sleep .1)
    (snap-image-and-decimate)
    (write-pgm16 (dir "snap~3,'0d.pgm" nuc) 
		 clara:*im*)
    (setf *bright-im* nil
	  *dark-im* t)))

#+nil
(dolist (e '(0 1 2 3))
  (capture-nucleus e))


;; OBTAIN
#+nil
(obtain-sectioned-slice)
#+nil
(progn
  (make-scaled-clara-im)
  (time
   (obtain-stack :n 8 :offset 1 :dz 2s0))
 (time (find-beads))
 nil)


(declaim (optimize (speed 1) (debug 3) (safety 3)))
;;; DRAW INTO OPENGL WINDOW (for LCOS and camera view)
(let* ((white-width 4)
       (phases-x 4)
       (phases-y 1)
       (a (make-array (* phases-x phases-y white-width) 
		      :element-type '(unsigned-byte 8)))
       (phase 0)
       
       ;(im-scale 100s0) (im-offset 1.86s0)
       (im-scale 2114s0) (im-offset 0.0s0)
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

	  #+nil (progn 	  ;; capture a dark image
		  (setf *dark-im* t) ;; dark LCOS
		  (select-disk *mma-dark*)
		  (sleep 1/60)
		  (snap-image-and-decimate)
		  (dotimes (j h)
		    (dotimes (i w)
		      (setf (aref dark-im i j) (aref clara:*im* i j))))
		  (write-pgm16 (dir "dark-~3,'0d.pgm" z)
			       dark-im))
	  ;; capture bright widefield image
	  (select-disk *mma-bright*)
	  (setf *dark-im* nil)
	  (setf *bright-im* t)
	  (sleep 1/60)
	  (snap-image-and-decimate)
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
	     (snap-image-and-decimate)
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
      (unwind-protect
	   (dotimes (k n)
	     (let ((z (+ center (* dz (- k offset)))))
	       (format t "slice ~f ~d/~d~%" z (1+ k) n)
	       (focus:set-position z)
	       (sleep .1)
	       (obtain-sectioned-slice :z k)
	       (push (list z *section-im*) result))
	     (when (= k (1- n))
	       (progn 	  ;; capture a dark image in last z-position
		  (setf *dark-im* t) ;; dark LCOS
		  (select-disk *mma-dark*)
		  (sleep 1/60)
		  (snap-image-and-decimate)
		  (write-pgm16 (dir "dark-~3,'0d.pgm" k)
			       clara:*im*))))
	(focus:set-position center))
      (setf *stack* (reverse result))))

  (defun draw-screen ()
    (gl:clear-color 0 0 0 1)
    (gl:clear :color-buffer-bit #+nil :depth-buffer-bit)
    ;(gl:enable :depth-test)
    
        
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

    (gl:with-pushed-matrix
      (gl:translate 10 100 0)
     (gl:with-pushed-matrix
       ;; TEX draw raw camera image on the left  
       (gl:scale .5 .5 1s0)
					;(gl:translate 20 600 .3)
       (when (and clara:*im* *scaled-clara-im*)
	 (let ((tex (make-instance 'gui::texture :data *scaled-clara-im*
				   )))
	   (destructuring-bind (h w) (array-dimensions clara:*im*)
	     (gui:draw tex :w (* 1s0 w) :h (* 1s0 h)
		       :wt 1s0 :ht 1s0))
	   (gui:destroy tex)))      )
     (let ((radius 25s0)) ;;FAN
       (when *bright-im*
	 (gl:with-pushed-matrix
	   (gl:translate *window-overlap* 0 0)
	   (gl:color 1 1 1)
	   (gl:line-width 3s0)
	   (gl:translate (vx *illum-target*) (- (vy *illum-target*) 168) 0.0)
	   (draw-disk-fan :radius radius))
	 (gl:with-pushed-matrix ;; small version for me to see
	   (gl:scale .5 .5 1s0)
	   (gl:color 1 .3 .2)
	   (macrolet ((tr (x y)
			`(lcos->cam (v ,x ,y 1))))
	     (let ((a (tr 0 0))
		   (b (tr 1280 1024))
		   (c (tr (vx *illum-target*)
			  (vy *illum-target*))))
	      (gl:with-primitive :line-loop
		(gl:vertex (vx a) (vy a))
		(gl:vertex (vx b) (vy a))
		(gl:vertex (vx b) (vy b))
		(gl:vertex (vx a) (vy b)))
					;(gl:color 1 1 1)
	      (gl:translate (vx c) (vy c) 0.0)
	     
	      (draw-disk-fan :radius 10s0)))))))
    
    (gl:check-error "draw")
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
  (when q
    (progn
     (setf *mma-select* q)
     (mma:select-pictures q :n 1 :ready-out-needed t))))

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
      (format s "P5~%~D ~D~%255~%" w h)) 
    ;; w h stores image in the correct orientation, when the lisp
    ;; array has the dimensions (list h w)
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
      (format s "P5~%~D ~D~%65535~%" w h))
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
