(in-package :gui)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *numerals*
    (let ((doc (cxml:parse-file "/home/martin/0505/mma/gui/numerals.svg" 
				(cxml-dom:make-dom-builder))))
      (loop for i below 10 collect
	   (list i 
		 (dom:get-attribute
		  (elt (dom:get-elements-by-tag-name doc "path") i) "d"))))
    "Parsed SVG document, that contains the characters 0123456789")

  (defun split (char str)
    "Split string STR into a list of strings at each occurrance of
CHAR."
    (let ((start 0))
      (loop as pos = (position char str :start start)
	 while pos collect
	   (progn
	     (setf start (1+ pos))
	     pos))))

  (defun split-at-comma-space (str)
    "Split a string whenever a comma or space is encountered, returns
list of strings without the delimeters."
    (let ((pos (sort (append (split #\Space str)
			     (split #\, str))
		     #'<)))
      (append 
       (list (subseq str 0 (elt pos 0)))
       (loop for i below (1- (length pos)) collect
	    (subseq str (1+ (elt pos i)) (elt pos (1+ i))))
       (list (subseq str (1+ (first (last pos))))))))

  (defparameter *svg-commands*
    '((r-translate 2)
      (translate 2)
      (r-cubic-bezier 6)
      (cubic-bezier 6)
      (r-line-to 2)
      (line-to 2)
      (end-loop 0))
    "Number of arguments to read after one-character SVG path
    instructions.")

  (defun svg-path-d-to-lisp (cmds)
    "Parse an SVG path definition like l -0.11,0.2 c 0.4,0 1.7,0
... into a list."
    (let ((last-cmd nil)
	  (res nil))
      (loop for i below (length cmds)
	 do
	   (let* ((current-cmd (cond ((string= "m" (elt cmds i)) 'r-translate)
				     #+nil ((string= "M" (elt cmds i)) 'translate)
				     ((string= "c" (elt cmds i)) 'r-cubic-bezier)
				     ((string= "C" (elt cmds i)) 'cubic-bezier)
				     ((string= "l" (elt cmds i)) 'r-line-to)
				     ((string= "L" (elt cmds i)) 'line-to)
				     ((string= "z" (elt cmds i)) 'end-loop)
				     (t  (decf i)
					 'repeat))))
	     (unless (eq current-cmd 'repeat)
	       (setf last-cmd current-cmd))
	     (let ((nr-args (first (cdr (assoc last-cmd *svg-commands*)))))
	       (push `(,last-cmd 
		       ,@(loop for j below nr-args collect 
			      (let ((arg (read-from-string (elt cmds (incf i)))))
					  (unless (numberp arg)
					    (break "parse error, expected number got ~a." arg))
					  arg)))
		     res))))
      (reverse res)))


  (defun expand-relative-bezier (args &key (n 10))
    "ARGS contains 3 control points (the first one is the
origin). Generate N points in between, that lie on the cubic bezier
path. The result is a list of relative shifts between those points and
can be fed into r-line-to."
    (destructuring-bind (x1 y1 x2 y2 x3 y3) args
      (let* ((ox 0s0)
	     (oy 0s0))
	(loop for i upto n collect
	     (let* ((u (/ (* 1s0 i) n))
		    (v (- 1s0 u))
		    (uu (* u u))
		    (vv (* v v))
		    (x (+ #+nil (* vv v x0) (* 3 vv u x1) (* 3 uu v x2) (* uu u x3)))
		    (y (+ #+nil (* vv v y0) (* 3 vv u y1) (* 3 uu v y2) (* uu u y3))))
	       (prog1
		   `(r-line-to ,(- x ox)
			       ,(- y oy))
		 (setf ox x
		       oy y)))))))

  #+nil
  (expand-relative-bezier '(-1.1 1.2 -2.5 1.3 -4 1.4))

  (defun expand-absolute-bezier (last-absolute-point args &key (n 10))
    "Generate N calls to LINE-TO on the cubic bezier path between
LAST-ABSOLUTE-POINT and the last point int ARGS. Using the first two
points in ARGS as control points."
    (destructuring-bind (cmd x0 y0) last-absolute-point
      (unless (eq cmd 'line-to)
	(break "unexpected command ~a, at this stage there should be only line-to" cmd))
      (destructuring-bind (x1 y1 x2 y2 x3 y3) args
	(loop for i upto n collect
	     (let* ((u (/ (* 1s0 i) n))
		    (v (- 1s0 u))
		    (uu (* u u))
		    (vv (* v v))
		    (x (+ (* vv v x0) (* 3 vv u x1) (* 3 uu v x2) (* uu u x3)))
		    (y (+ (* vv v y0) (* 3 vv u y1) (* 3 uu v y2) (* uu u y3))))
	       `(line-to ,x ,y))))))

  (defun expand-all-relative-bezier-into-lines (ls &key (n 10))
    "Replace all occurances of R-CUBIC-BEZIER with N calls to
R-LINE-TO (each)."
    (let ((res nil))
      (dolist (e ls)
	(destructuring-bind (cmd &rest rest) e
	  (cond ((eq cmd 'r-cubic-bezier)
		 (let ((lines (expand-relative-bezier rest :n n)))
		   (dolist (f lines)
		     (push f res))))
		(t (push e res)))))
      (reverse res)))
  
  (defun accumulate-relative-coordinates (ls)
    "Go through a list containing relative drawing commands like
R-LINE-TO and accumulate into absolute drawing commands only."
    (let ((res nil)
	  (x 0s0)
	  (y 0s0))
      (dolist (e ls)
	(destructuring-bind (cmd &rest rest) e
	  (cond ((eq cmd 'r-line-to)
		 (destructuring-bind (xx yy) rest
		   (incf x xx)
		   (incf y yy)
		   (push `(line-to ,x ,y) res)))
		((eq cmd 'line-to)
		 (destructuring-bind (xx yy) rest
		   (setf x xx)
		   (setf y yy)
		   (push `(line-to ,x ,y) res)))
		((eq cmd 'r-translate)
		 (destructuring-bind (xx yy) rest
		   (incf x xx)
		   (incf y yy)
		   (push `(translate ,x ,y) res)))
		((eq cmd 'cubic-bezier)
		 (dolist (e (expand-absolute-bezier (first res) rest))
		   (push e res))
		 (setf x (second (first res))
		       y (third (first res))))
		((eq cmd 'end-loop))
		(t (break "unexpected command ~a" cmd)))))
      (reverse res)))

  (defun svg-to-immediate-opengl (ls)
    "Replace all LINE-TO commands with VERTEX for drawing with OpenGL."
    (let ((res))
      (dolist (e ls)
	(destructuring-bind (cmd x y) e
	  (cond ((eq cmd 'line-to)
		 (push `(vertex ,x ,y) res))
		((or (eq cmd 'translate))
		 (push `(vertex ,x ,y) res))
		(t (break "unexpected command ~a" cmd)))))
      (reverse res))))

(defmacro def-number-fun ()
  "Expand into 10 functions that will draw digits from the SVG data."
  `(progn
     ,@(loop for i from 0 below 10 collect
	    (let ((path-data (first (cdr (assoc i *numerals*)))))
	      `(defun ,(intern (format nil "DRAW-~a" i)) ()
		 ,@(svg-to-immediate-opengl
		    (accumulate-relative-coordinates 
		     (expand-all-relative-bezier-into-lines 
		      (svg-path-d-to-lisp (split-at-comma-space path-data))
		      :n 6))))))
     (defun draw-digit (c)
       (ecase c
	 ,@(loop for i from 0 below 10 collect
		`(,i (,(intern (format nil "DRAW-~a" i)))))))))

(def-number-fun)

(defun draw-centered-char (i)
  "The digits in the SVG are next to each other, this function shifts
them ontop of each other."
  (with-pushed-matrix 
   (translate (* -11 i) 0 0)
   (with-primitive :line-loop
     (draw-digit i))))

(defun draw-number (val)
  "Draw an integer number on the screen using the SVG vector font. One
character is 11 wide and 24 high."
  (declare ((integer 0) val))
  (let ((s (format nil "~a" val)))
    (with-pushed-matrix
      (scale 1 -1 1)
      (translate 0 -1040 0)
      (dotimes (i (length s))
	(translate 11 0 0)
	(draw-centered-char (- (char-code (char s i))
			       (char-code #\0)))))))