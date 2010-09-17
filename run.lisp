#.(require :gui)

(let ((a (make-array (* 12 3) :element-type '(unsigned-byte 8))))
  (dotimes (i (length a))
    (setf (aref a i) (mod i 255)))
  (defun draw ()
    (gl:clear-color .4 .4 0 1)
    (gl:clear :color-buffer-bit)
    (gl:translate 32 23 0)
    (gui::with-grating (g a)
      (gui::draw g :w 256.0 :h 256.0))
    (gl:color 0 0 0)
    (gl:with-primitive :lines
      (gl:vertex 0 0) (gl:vertex 0 100)
      (gl:vertex 0 0) (gl:vertex 100 122)
      (gl:vertex 0 0) (gl:vertex 100 0))))

#+nil
(gui:with-gui
  (draw))