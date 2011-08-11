(in-package :acquisitor)

(defun plan-stack (&key (slices 1) lcos-seq (start-pos 0) (dz 1)
		   (frame-period (/ 60)) (start-time 0d0)
		   (stage-settle-duration 20) (lcos-lag 0))
  (let ((res nil)
        (pos start-pos)
        (time (+ start-time frame-period))
	(image-index 0))
    (push (list :type :capture
                :start 0
                :end 15
                :content '(10 :dark)
		:image-index image-index)
          res)
    (loop for k below slices do
         (let ((cam nil))
          (push (list :type :lcos-display
                      :pos pos
                      :lcos-seq
                      (let ((lcos nil)
			    (phase 0))
                        (loop for e in lcos-seq do
			     (let ((ee (list (+ (* 1000 k) ) e)))
			      ;; lcos displays each frame twice
			      (push (list :start time 
					  :end (+ time 15)
					  :content (list (+ (* 1000 k) ) e)) 
				    lcos)
			      (incf time frame-period)
			      (push (list :start time
					  :end (+ time 15)
					  :content ee)
				    lcos)
			      ;; one of the frames is captured by the camera
			      (push (list :type :capture
					  :start time
					  :end (+ time 15)
					  :content e
					  :slice k
					  :image-index (incf image-index))
				    cam)
			      ;; for this frame the MMA was white
			      (push (list :type :mma
					  :start time
					  :end (+ time 15)
					  :content e)
				    cam)
			      (incf time frame-period)))
                        (reverse lcos)))
                res)
          (loop for e in (reverse cam) do
               (push e res))
          (incf pos dz)
          (unless (= k (- slices 1))
           (push (list :type :stage-move
                       :start (+ time frame-period) ;; move stage in one of the dark images
                       :end (+ time frame-period stage-settle-duration (- lcos-lag))
                       :pos pos)
                 res))))
    (reverse res)))

;; lcos is always a list of things to be displayed, it can be
;; '(:dark), '(:bright) or a list of primitives: '((:grating-disk 120
;; 132 10 2 0) (:grating-disk 230 232 40 2 0)). The primitives are
;; (:grating-disk x y r phases phase) and (:disk x y r))

;; mma has additionally the primitive (:triangles )

(defun make-exposure (&key (lcos '((:dark))) (mma '((:dark))) (accum-group 0))
  (let ((r nil))
    (setf (getf r :lcos) lcos
	  (getf r :mma) mma
	  (getf r :accum-group) accum-group)
    (list :exposure r)))

(defun check-exposure (c)
  (unless (eq :exposure (first c))
    (break "Object not of type :exposure ~a." (first c))))

(defun get-exposure (place indicator)
  (declare (type (member :lcos :mma :accum-group) indicator))
  (check-exposure place)
  (getf (second place) indicator))

#+nil
(get-exposure (make-exposure) :mma)
#+nil
(get-exposure
 (make-exposure :lcos '((grating-disk 12 12 30 3 0)
			(grating-disk 23 34 30 3 0))
		:mma '((:bright))
		:accum-group 3)
 :lcos)

(defun plan-full-grating-stack (&key (slices 10) (phases 3) (repetition 2) (start-pos 0) (dz 1)
				(frame-period (/ 60))
				(start-time 0d0) (stage-settle-duration 20)
				(lcos-lag 0))
  (let ((res nil)
        (pos start-pos)
        (time (+ start-time frame-period))
	(image-index 0))
    (push (list :type :capture
                :start 0
                :end 15
                :content (make-exposure :accum-group 1)
		:image-index image-index)
          res)
    (loop for k below slices do
         (let ((cam nil))
          (push (list :type :display
                      :pos pos
                      :lcos-seq
                      (let ((lcos nil)
			    (phase 0))
                        (loop for e in (list :dark ) do
			     (let ((ee (list (+ (* 1000 k) ) e)))
			      ;; lcos displays each frame twice
			      (push (list :start time 
					  :end (+ time 15)
					  :content (list (+ (* 1000 k) ) e)) 
				    lcos)
			      (incf time frame-period)
			      (push (list :start time
					  :end (+ time 15)
					  :content ee)
				    lcos)
			      ;; one of the frames is captured by the camera
			      (push (list :type :capture
					  :start time
					  :end (+ time 15)
					  :content e
					  :slice k
					  :image-index (incf image-index))
				    cam)
			      ;; for this frame the MMA was white
			      (push (list :type :mma
					  :start time
					  :end (+ time 15)
					  :content e)
				    cam)
			      (incf time frame-period)))
                        (reverse lcos)))
                res)
          (loop for e in (reverse cam) do
               (push e res))
          (incf pos dz)
          (unless (= k (- slices 1))
           (push (list :type :stage-move
                       :start (+ time frame-period) ;; move stage in one of the dark images
                       :end (+ time frame-period stage-settle-duration (- lcos-lag))
                       :pos pos)
                 res))))
    (reverse res)))

#+nil
(plan-stack :slices 2 :lcos-seq '((10 :dark) ()))

(defun extract-moves (ls)
  (mapcar #'(lambda (x) (getf x :start)) 
          (remove-if-not #'(lambda (x) (eq :stage-move (getf x :type)))
                         ls)))

(defparameter *stack-state* nil)


(defmacro ss (sym)
  "Access an entry in the stack state"
  `(getf acquisitor::*stack-state* ,sym))

(defun prepare-stack-acquisition (&key (slices 10) (dz 1) (lcos-seq '(:dark 0 1 2)))
  (when (ss :wait-move-thread) 
    (close-move-thread))
  (let* ((start-pos (focus:get-position))
	 (seq (plan-stack :slices slices 
			  :lcos-seq lcos-seq
			  :frame-period (/ 1000 60)
			  :start-pos start-pos :dz dz))
	 (moves (extract-moves seq)))
    (setf *stack-state* nil)
     (setf (ss :seq) seq
	   (ss :moves) moves ;; planned moves
	   (ss :real-moves) nil ;; times when actual stage movements occured
	   (ss :start) 0
	   (ss :phases) (1- (length lcos-seq))
	   (ss :slices) slices
	   (ss :do-wait-move) t
	   (ss :wait-move-thread) nil
	   (ss :start-position) start-pos
	   (ss :set-start-at-next-swap-buffer) nil
	   (ss :image-array) nil
	   (ss :image-times) nil)))


(prepare-stack-acquisition)


(defun close-move-thread ()
  (let ((thread (ss :wait-move-thread)))
    (when thread
      (setf (ss :do-wait-move) nil)
      (handler-case (sb-thread:join-thread thread)
	(sb-thread:join-thread-error () (format t "Note: no thread woke up")))
      (setf (ss :do-wait-move) t))))

(defun previous-move (time ls)
  (first (last (remove-if #'(lambda (x) (< time x)) ls))))

(defun next-move (time ls)
  (first (remove-if #'(lambda (x) (< x time)) ls)))

#+nil
(next-move 320 (ss :moves))

(defun next-position (time)
  (getf (first (remove-if #'(lambda (x) (< (getf x :start) time))
			  (mapcar #'(lambda (x) (let ((y nil)) 
					     (setf (getf y :start) (getf x :start)
						   (getf y :pos) (getf x :pos))
					     y)) 
				  (remove-if-not #'(lambda (x) (eq :stage-move (getf x :type)))
						 (ss :seq)))))
	:pos))
#+nil
(next-position 304)

(defun clear-real-moves ()
  (prog1
      (ss :real-moves)
    (setf (ss :real-moves) nil)))

#+nil
(clear-real-moves)

(defun move-stage-fun ()
  (loop while (ss :do-wait-move) do
       (let ((start (ss :start)))
	 (if (= start 0)
	     (sleep (/ 2 1000))
	     (let* ((time (- (get-internal-real-time) start))
		    (next (or (next-move time (ss :moves))
			      (progn 
				(push time (ss :real-moves))
				(setf (ss :real-moves) (reverse (ss :real-moves)))
				(sleep (* (/ 60) (1+ (ss :phases)))) ;; wait for all images
				(focus:set-position (ss :start-position))
				(return-from move-stage-fun
				  (+ 1 time)))))
		    (diff (- next time)))
	    (when (< 2 diff)
	      (push time (ss :real-moves))
	      (let ((npos (next-position time)))
		(format t "~a~%" (list diff time next npos))
		(focus:set-position npos))
	      (sleep (/ diff 1000))))))))

(defun start-move-thread ()
    (close-move-thread)
    (setf (ss :wait-move-thread)
	  (sb-thread:make-thread #'(lambda () (move-stage-fun))
				 :name "stage-mover")))

(defun get-lcos-sequence ()
 (let ((res nil))
   (dolist (es (mapcar #'(lambda (x) (mapcar #'(lambda (y) (getf y :content))
					(getf x :lcos-seq)))
		       (remove-if-not #'(lambda (x) (eq :lcos-display (getf x :type))) (ss :seq))))
 
     (dolist (e es)
       (push e res)))
   (reverse res)))

(defun get-capture-sequence ()
 (remove-if-not #'(lambda (x) (eq :capture (getf x :type))) (ss :seq)))

(defun acquire-stack (&key (show-on-screen nil) (slices 10) (dz 1) (width 4) (lcos-seq '(:dark 0 1 2)))
  (unless show-on-screen 
    (setf run-gui::*do-capture* nil
	  run-gui::*do-display-queue* nil)
    (sleep .1)
    (setf run-gui::*do-capture* t)
    (clara::abort-acquisition )
    (clara::prepare-acquisition)
    (setf run-gui::*line* (sb-concurrency:make-queue :name 'picture-fifo)))

  (prepare-stack-acquisition :slices slices :dz dz :lcos-seq lcos-seq)
  (let ((phases (ss :phases)))
    (dolist (e (get-lcos-sequence))     
      (unless (eq e :dark)
	(run-gui::lcos (format nil "qgrating-disk 425 325 200 ~d ~d ~d" 
		      e phases width)))
      (run-gui::lcos "qswap")))

  (let ((img-array (make-array (length (get-capture-sequence))))
	(img-time (make-array (length (get-capture-sequence)))))
    (run-gui::lcos "toggle-queue 1")
    (setf (ss :set-start-at-next-swap-buffer) t)
    (unless show-on-screen (clara:start-acquisition)) ;; start camera
    
    (start-move-thread) ;; I could start this in the opengl drawing loop
    
    (unless show-on-screen (let ((count 0))
			     (loop while (and run-gui::*do-capture*
					      (< count (length img-array))) do
				  (run-gui::capture)
				  (loop for i below (sb-concurrency:queue-count run-gui::*line*) do
				       (setf (aref img-array count) (sb-concurrency:dequeue run-gui::*line*)
					     (aref img-time count) (get-internal-real-time))
				       (incf count)))
			     (clara:abort-acquisition)
			     (clara:free-internal-memory))
	    (setf (ss :image-array) img-array 
		  (ss :image-time) img-time)))

  (setf run-gui::*do-display-queue* t))


(defun get-dark-indices ()
  (mapcar #'(lambda (x) (getf x :image-index))
	  (remove-if-not #'(lambda (x) (eq :dark (getf x :content)))
			 (get-capture-sequence))))

#+nil
(get-dark-indices)

(defun accumulate-dark-images ()
  (destructuring-bind (y x) (array-dimensions (elt (ss :image-array) 0))
   (let* ((darki (get-dark-indices))
	  (1/n (/ 1s0 (length darki)))
	  (dark (make-array (list y x) :element-type '(single-float))))
     (dolist (e darki)
       (let ((a (elt (ss :image-array) e)))
	(vol:do-region ((j i) (y x))
	  (incf (aref dark j i) (* 1/n (aref a j i))))))
     dark)))

#+nil
(vol::write-pgm-transposed "/dev/shm/o.pgm"
			   (vol:normalize-2-sf/ub8 (accumulate-dark-images)))

(defun encode-phase-hash (slice phase)
  "Encode phase and slice into a hash key."
  (assert (< (ss :phases) 100))
  (+ (* 100 slice)
     phase))
#+nil
(encode-phase-hash 12 3)

(defun decode-phase-hash (key)
  "Retrun slice and phase of a hash key."
  (values (floor key 100)
	  (mod key 100)))
#+nil
(decode-phase-hash 312)

(defun put-phases-into-hash ()
  (let ((tbl (make-hash-table)))
   (mapcar #'(lambda (x)
	       (setf (gethash (encode-phase-hash (getf x :slice) 
						 (getf x :content))
			      tbl)
		     (getf x :image-index)))
	   (remove-if #'(lambda (x) (eq :dark (getf x :content)))
		      (get-capture-sequence)))
   tbl))
#+nil
(defparameter *qee*
 (get-dark-indices))

#+nil
(defparameter *hsh* (put-phases-into-hash))

(defun reconstruct-from-phase-images (&key (algorithm :max-min))
  (declare (type (member :max-min :sqrt) algorithm))
  (let ((hsh (put-phases-into-hash))
	(phases (ss :phases))
	(slices (ss :slices))
	(res nil)
	(ia (ss :image-array)))
    (assert (= phases 3))
    (destructuring-bind (y x) (array-dimensions (elt ia 0))
      (dotimes (i slices)
	(let ((r (make-array (list y x) :element-type 'single-float))
	      (a0 (elt ia (gethash (encode-phase-hash i 0) hsh)))
	      (a1 (elt ia (gethash (encode-phase-hash i 1) hsh)))
	      (a2 (elt ia (gethash (encode-phase-hash i 2) hsh))))
	  (macrolet ((a (ar) `(aref ,ar j i)))
	    (case algorithm
	      (:sqrt
	       (vol:do-region ((j i) (y x))
		 (setf (a r)
		       (sqrt (+ (expt (- (a a0) (a a1)) 2)
				(expt (- (a a1) (a a2)) 2)
				(expt (- (a a0) (a a2)) 2))))))
	      (:max-min
	       (vol:do-region ((j i) (y x))
		 (setf (a r)
		       (* 1s0 (- (max (a a0) (a a1) (a a2))
				 (min (a a0) (a a1) (a a2)))))))))
	  (push r res))))
    (reverse res)))

#+nil
(loop for e in (reconstruct-from-phase-images)
     for i = 0 then (1+ i) do
     (vol::write-pgm-transposed (format nil "/dev/shm/o~4,'0d.pgm" i)
				(vol:normalize-2-sf/ub8 e)))

#+nil 
(dotimes (i (length (ss :image-array)))
  ;; store images
  (vol::write-pgm-transposed 
   (format nil "/dev/shm/o~4,'0d.pgm" i)
   (vol:normalize-2-sf/ub8
    (vol:convert-2-ub16/sf-mul (aref (ss :image-array) i)))))

(defun draw-moves ()
  (flet ((vline (x &optional (y0 0) (y1 80))
	   (with-primitive :lines
	     (vertex x y0)
	     (vertex x y1))))
   (with-pushed-matrix 
     (color 0 0 0 1)
     (rect 0 0 1000 80)
     (scale .19 1 1)
     (color 1 1 1)
     (vline (- (get-internal-real-time) (ss :start)) 60 80)
     (color .3 1 .3)
     
     (dolist (e (ss :real-moves))
       (vline e))
     
     (let ((it (ss :image-time)))
       (when it
	 (dotimes (i (length it))
	   (case (getf (elt (get-capture-sequence) i)
		       :content)
	     (:dark (color .4 .4 .4))
	     (0 (color .8 .0 .2))
	     (1 (color .1 .9 .2))
	     (2 (color .1 .2 1)))
	   (vline (- (aref it i) (ss :start))
		  0 20))))
     
     (translate 0 20 0)
     (dolist (e (ss :seq))
       (case (getf e :type)
	 (:capture
	  (apply #'color (case (getf e :content)
			   (:dark (list .4 .4 .4))
			   (0 (list .9 .1 .1))
			   (1 (list 0 .7 0))
			   (2 (list .2 .2 1))))
	  (rect (getf e :start) 0 (getf e :end) 20))
	 (:stage-move
	  (color 1 1 1)
	  (rect (getf e :start) 21 (getf e :end) 41)))))
   
   (when (ss :set-start-at-next-swap-buffer)
     ;; this code has to be executed to synchronize stage with display
     (setf (ss :set-start-at-next-swap-buffer) nil
	    (ss :start) (get-internal-real-time)))))
