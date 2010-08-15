(in-package :vol)

;; bbox contains float values but is also used to represent pixel
;; positions. In that case start is the first sample that is non-zero
;; and end is the last non-zero pixel.
(defstruct bbox
  (start (v) :type vec)
  (end (alexandria:required-argument) :type vec))

(def-generator (extract-bbox (rank type))
  `(defun ,name (a bbox)
     (declare ((simple-array ,long-type ,rank) a)
	      (bbox bbox)
	      (values (simple-array ,long-type ,rank) &optional))
     ,(ecase
       rank 
       (2 `(destructuring-bind (y x) (array-dimensions a)
	     (with-slots (start end) bbox
	       (unless (and (< (vec-x end) x) (< (vec-y end) y))
		 (error "bbox is bigger than array"))
	       (let* ((sx (floor (vec-x start)))
		      (sy (floor (vec-y start)))
		      (widths (v+ (v- end start) (v 1d0 1d0)))
		      (res (make-array (list (floor (vec-y widths))
					     (floor (vec-x widths)))
				       :element-type ',long-type)))
		 (destructuring-bind (yy xx)
		     (array-dimensions res)
		   (do-region ((j i) (yy xx))
		     (setf (aref res j i) (aref a (+ j sy) (+ i sx)))))
		 res))))
       (3 `(destructuring-bind (z y x) (array-dimensions a)
	     (with-slots (start end) bbox
	       (unless (and (< (vec-x end) x) (< (vec-y end) y) (< (vec-z end) z))
		 (error "bbox is bigger than array"))
	       (let* ((sx (floor (vec-x start)))
		      (sy (floor (vec-y start)))
		      (sz (floor (vec-z start)))
		      (widths (v+ (v- end start) (v 1d0 1d0 1d0)))
		      (res (make-array (list (floor (vec-z widths))
					     (floor (vec-y widths))
					     (floor (vec-x widths)))
				       :element-type ',long-type)))
		 (destructuring-bind (zz yy xx)
		     (array-dimensions res)
		   (do-region ((k j i) (zz yy xx))
		     (setf (aref res k j i)
			   (aref a (+ k sz) (+ j sy) (+ i sx)))))
		 res)))))))

(defmacro def-extract-box-functions (ranks types)
  (let ((specifics nil)
	(cases nil)
	(name (format-symbol "extract-bbox")))
    (loop for rank in ranks do
	 (loop for type in types do
	      (push `(def-extract-bbox-rank-type ,rank ,type)
		    specifics)
	      (push `((simple-array ,(get-long-type type) ,rank)
		      (,(format-symbol "~a-~a-~a" name rank type) a bbox))
		    cases)))
    (store-new-function name)
    `(progn ,@specifics
	    (defun ,name (a bbox)
	       (etypecase a
		 ,@cases
		 (t (error "The given type can't be handled with a generic ~a function." ',name)))))))

(def-extract-box-functions (2 3) (ub8 sf df csf cdf))



(def-generator (replace-bbox (rank type))
  `(defun ,name (a b bbox)
     "A beeing a big array, and B a smaller one with BBOX giving its
coordinates relative to A, replace the contents of A with B."
     (declare ((simple-array ,long-type ,rank) a b)
	      (bbox bbox)
	      (values (simple-array ,long-type ,rank) &optional))
     ,(ecase 
       rank
       (2 `(destructuring-bind (y x) (array-dimensions a)
	     (destructuring-bind (yy xx) (array-dimensions b)
	       (with-slots (start end) bbox
		 (unless (and (< (vec-x end) x) (< (vec-y end) y))
		   (error "bbox is bigger than array"))
		 (let* ((widths (v+ (v- end start) (v 1d0 1d0))))
		   (unless (and (= (floor (vec-x widths)) xx)
				(= (floor (vec-y widths)) yy))
		     (error "size of BBOX isn't the same as size of small array B"))
		   (let ((sx (floor (vec-x start)))
			 (sy (floor (vec-y start))))
		     (do-region ((j i) (yy xx))
		       (setf (aref a (+ sy j) (+ sx i)) (aref b j i)))
		     a))))))
       (3 `(destructuring-bind (z y x) (array-dimensions a)
	     (destructuring-bind (zz yy xx) (array-dimensions b)
	       (with-slots (start end) bbox
		 (unless (and (< (vec-x end) x) (< (vec-y end) y) (< (vec-z end) z))
		   (error "bbox is bigger than array"))
		 (let ((widths (v+ (v- end start) (v 1d0 1d0 1d0))))
		   (unless (and (= (floor (vec-x widths)) xx)
				(= (floor (vec-y widths)) yy)
				(= (floor (vec-z widths)) zz))
		     (error "size of BBOX isn't the same as size of small array B"))
		   (let ((sx (floor (vec-x start)))
			 (sy (floor (vec-y start)))
			 (sz (floor (vec-z start))))
		     (do-region ((k j i) (zz yy xx))
		       (setf (aref a (+ sz k) (+ sy j) (+ sx i)) (aref b k j i)))
		     a)))))))))
#+nil
(def-replace-bbox-rank-type 2 ub8)
#+nil
(let* ((a (make-array (list 3 3) :element-type '(unsigned-byte 8)))
       (b (make-array (list 2 2) :element-type '(unsigned-byte 8)))
       (b1 (sb-ext:array-storage-vector b)))
  (dotimes (i (length b1))
    (setf (aref b1 i) i))
  (replace-bbox-2-ub8 a b (make-bbox :start (v) :end (v 1d0 1d0))))

(defmacro def-replace-bbox-functions (ranks types)
  (let* ((specifics nil)
	 (cases nil)
	 (name (format-symbol "replace-bbox")))
    (loop for rank in ranks do
	 (loop for type in types do
	      (let ((def-name (format-symbol "def-~a-rank-type" name))
		    (specific-name (format-symbol "~a-~a-~a" name rank type)))
		(push `(,def-name ,rank ,type) specifics)
		(push `((simple-array ,(get-long-type type) ,rank)
			(,specific-name a b bbox))
		      cases))))
    (store-new-function name)
    `(progn ,@specifics
	    (defun ,name (a b bbox)
	       (etypecase a
		 ,@cases
		 (t (error "The given type can't be handled with a generic ~a function." ',name)))))))

(def-replace-bbox-functions (2 3) (ub8 sf df csf cdf))




(def-generator (find-bbox (rank type))
  `(defun ,name (a)
     "A beeing a big array, and B a smaller one with BBOX giving its
coordinates relative to A, replace the contents of A with B."
     (declare ((simple-array ,long-type ,rank) a)
	      (values (or null bbox) &optional))
     ,(ecase 
       rank
       (2 `(destructuring-bind (y x)
	       (array-dimensions a)
	     (labels ((top () 
			;; Note: the order of the loops is important
			(do-region ((j i) (y x))
			  (unless (= 0 (aref a j i)) (return-from top j)))
			(1- y))
		      (left () ;; search from left side for first non-zero
			(do-region ((i j) (x y)) (unless (= 0 (aref a j i))
			    (return-from left i)))
			(1- x))
		      (bottom ()
			(do-region ((j i) (y x))
			  ;; invert j so that it starts search from bottom
			  (let ((jj (- (1- y) j))) (unless (= 0 (aref a jj i))
						     (return-from bottom jj))))
			0)
		      (right () 
			(do-region ((i j) (x y))
			  (let ((ii (- (1- x) i))) (unless (= 0 (aref a j ii))
						     (return-from right ii))))
			0))
	       (let ((l (left))
		     (r (right)))
		 (when (<= l r) ;; otherwise all pixels are zero
		   (make-bbox :start (v (* 1d0 l) (* 1d0 (top)))
			      :end (v (* 1d0 r) (* 1d0 (bottom)))))))))
       (3 `(destructuring-bind (z y x) (array-dimensions a)
	     (labels ((front ()
			(do-region ((k j i) (z y x)) (unless (= 0 (aref a k j i))
						       (return-from front k)))
			(1- z))
		      (back ()
			(do-region ((k j i) (z y x))
			  (let ((kk (- (1- z) k))) (unless (= 0 (aref a kk j i))
						     (return-from back kk))))
			0)
		      (top () 
			(do-region ((j k i) (y z x)) (unless (= 0 (aref a k j i))
						       (return-from top j)))
			(1- y))
		      (left ()
			(do-region ((i k j) (x z y)) (unless (= 0 (aref a k j i))
						       (return-from left i)))
			(1- x))
		      (bottom ()
			(do-region ((j k i) (y z x)) 
			  (let ((jj (- (1- y) j))) (unless (= 0 (aref a k jj i))
						     (return-from bottom jj))))
			0)
		      (right () 
			(do-region ((i k j) (x z y))
			  (let ((ii (- (1- x) i))) (unless (= 0 (aref a k j ii))
						     (return-from right ii))))
			0))
	       (let ((l (left))
		     (r (right)))
		 (when (<= l r) ;; otherwise all pixels are zero
		   (make-bbox :start (v (* 1d0 l) (* 1d0 (top)) (* 1d0 (front)))
			      :end (v (* 1d0 r) (* 1d0 (bottom)) (* 1d0 (back))))))))))))
#+nil
(def-find-bbox-rank-type 2 ub8)

(defmacro def-find-bbox-functions (ranks types)
  (let* ((specifics nil)
	 (cases nil)
	 (name (format-symbol "find-bbox")))
    (loop for rank in ranks do
	 (loop for type in types do
	      (let ((def-name (format-symbol "def-~a-rank-type" name))
		    (specific-name (format-symbol "~a-~a-~a" name rank type)))
		(push `(,def-name ,rank ,type) specifics)
		(push `((simple-array ,(get-long-type type) ,rank)
			(,specific-name a))
		      cases))))
    (store-new-function name)
    `(progn ,@specifics
	    (defun ,name (a)
	       (etypecase a
		 ,@cases
		 (t (error "The given type can't be handled with a generic ~a function." ',name)))))))

(def-find-bbox-functions (2 3) (ub8 sf df csf cdf))

#+nil
(let* ((a (make-array (list 5 5) 
		      :element-type '(unsigned-byte 8)
		      :initial-contents '((0 0 0 0 0)
					  (0 1 0 0 0)
					  (0 0 0 1 0)
					  (0 0 0 0 0)
					  (0 0 0 0 0))))
       (empty (make-array (list 5 5) 
			  :element-type '(unsigned-byte 8)))
       (box (find-bbox a))
       (ex (extract-bbox a box)))
  (list box ex (replace-bbox empty ex box)))

#+nil
(let* ((empty (make-array (list 4 4 4) :element-type '(unsigned-byte 8)))
       (a (make-array (list 4 4 4) 
		      :element-type '(unsigned-byte 8)
		      :initial-contents
		      '(((0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0))
			((0 0 0 0)
			 (0 1 0 0)
			 (0 0 1 0)
			 (0 0 0 0))
			((0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0))
			((0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)
			 (0 0 0 0)))))
       (box (find-bbox-3-ub8 a))
       (ex (extract-bbox-3-ub8 a box)))
  (list box ex q(replace-bbox-3-ub8 empty ex box)))
