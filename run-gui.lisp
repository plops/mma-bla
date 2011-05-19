(require :gui)
(require :clara)

(defparameter *blub*
  (let* ((w 1392)
	 (h 1040)
	 (img (make-array (list h w)
			  :element-type '(unsigned-byte 16)))
	 (img1 (sb-ext:array-storage-vector img))
	 (sap (sb-sys:vector-sap img1)))
    (progn
      (start-acquisition)
      (loop while (not (eq 'DRV_IDLE
			   (lookup-error (val2 (get-status)))))
	 do
	   (sleep .01))
      (sb-sys:with-pinned-objects (img)
	(get-acquired-data16 sap (length img1)))
      (check
	(free-internal-memory)))
   img))

(let ((bla 'e))
 (defun draw-screen ()
   (gl:clear-color 0 0 0 1)
   (gl:clear :color-buffer-bit)
   (gl:line-width 30)
   (gl:color 1 1 1)
   (gl:rect 500 1200 800 1900)
   (gl:with-primitive :lines
     (gl:vertex 100 100)
     (gl:vertex 200 100)
     (gl:vertex 200 500)
     (gl:vertex 100 500))))

(sb-thread:make-thread 
 #'(lambda ()
     (gui:with-gui (1280 (* 2 1024))
       (draw-screen)))
 :name "display-gui")