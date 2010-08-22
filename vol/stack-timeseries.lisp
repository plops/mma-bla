(defun split-by-char (char string)
    "Returns a list of substrings of string
divided by ONE character CHAR each.
Note: Two consecutive CHAR will be seen as
if there were an empty string between them."
    (declare (character char)
	     (string string))
    (loop for i = 0 then (1+ j)
       as j = (position char string :start i)
       collect (subseq string i j)
       while j))

#+nil
(split-by-char #\x "12x124x42")

(defun parse-raw-filename (fn)
  "Parses a filename like
/home/martin/d0708/stacks/c-291x354x41x91_dx200_dz1000.raw and returns
291 354 41 and 91 as multiple values."
  (declare (string fn)
	   (values (or null fixnum) fixnum fixnum fixnum &optional))
  (let* ((p- (position #\- fn :from-end t))
	 (part (subseq fn (1+ p-)))
	 (p_ (position #\_ part))
	 (sizes (subseq part 0 p_))
	 (numlist (split-by-char #\x sizes)))
    (unless (eq 4 (length numlist))
      (error "didn't read 4 dimensions as expected."))
    (destructuring-bind (x y z time)
	(mapcar #'read-from-string numlist)
     (values x y z time))))


#+nil
(parse-raw-filename "/home/martin/d0708/stacks/c-291x354x41x91_dx200_dz1000.raw")

(defun read-raw-stack-video-frame (fn time)
  (declare (string fn)
	   (fixnum time)
	   (values (simple-array (unsigned-byte 8) 3) &optional))
  (multiple-value-bind (x y z maxtime)
      (parse-raw-filename fn)
      (unless (< time maxtime)
	(error "requested time ~d is to big (must be <~d!)" time maxtime))
      (let* ((vol (make-array (list z y x)
			      :element-type '(unsigned-byte 8)))
	     (vol1 (sb-ext:array-storage-vector vol)))
	(with-open-file (s fn :direction :input
			   :element-type '(unsigned-byte 8))
	  (file-position s (* x y z time))
	  (read-sequence vol1 s))
	vol)))
#+nil
(time 
 (let* ((fn "/home/martin/d0708/stacks/c-291x354x41x91_dx200_dz1000_2.raw")
	(ao (decimate-xy-ub8 5
			     (read-raw-stack-video-frame fn 0))))
   (destructuring-bind (z y x)
       (array-dimensions ao)
     (let* ((timestep 20)
	    (o (loop for radius from 1 upto 10 collect
		    (let* ((oval (draw-sphere-ub8 (* 1d0 radius) z y x))
			   (volume (count-non-zero-ub8 oval)))
		      (list radius volume
			    (ft3 (convert3-ub8/cdf-complex oval)))))))
       (let* ((ao (decimate-xy-ub8 5
				   (read-raw-stack-video-frame fn timestep)))
	      (a (convert3-ub8/cdf-complex ao))
	      (ka (ft3 a)))
	 (loop for i in o do
	      (destructuring-bind (radius volume oval)
		  i
		(let* ((dir (format nil "/home/martin/tmp/o~d" radius))
		       (conv (fftshift3 (ift3 (.* ka oval))))
		       (conv-df (convert3-cdf/df-realpart conv)))
		 (save-stack-ub8 dir
				 (normalize-ub8-df/ub8-realpart conv-df))
		 (with-open-file (s (format nil "~a/maxima" dir)
				    :if-exists :supersede
				   :direction :output)
		   (loop for el in (find-maxima3-df conv-df) do
			(destructuring-bind (height pos)
			    el
			  (format s "~f ~d ~a~%" 
				  (/ height volume)
				  volume
				  (v*-i 
				   (map 'vec-i #'(lambda (x) (floor x 2)) pos)
				   2))))))))
	nil)))))
