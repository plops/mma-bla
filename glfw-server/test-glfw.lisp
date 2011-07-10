
(defparameter *lcos-chan* nil)
(defun lcos (cmd)
  (let ((s (sb-ext:process-input *lcos-chan*)))
    (format s "~a~%" cmd)
    (finish-output s)))


#+nil
(progn
  (sb-posix:setenv "DISPLAY" ":0" 1)
  (setf *lcos-chan*
        (sb-ext:run-program "/home/martin/0505/mma/glfw-server/glfw" '("400" "400")
                            :output :stream
                            :input :stream
                            :wait nil))
  
  (sb-thread:make-thread 
   #'(lambda ()
       (unwind-protect
           (with-open-stream (s (sb-ext:process-output *lcos-chan*))
             (loop for line = (read-line s nil nil)
                while line do
                  (format t "lcos read: ~a~%" line)
                  (finish-output)))
         (sb-ext:process-close *lcos-chan*)))
   :name "cmd-reader"))
#+nil
(lcos "toggle-stripes 1")
#+nil
(lcos "quit")
#+nil
(let ((a (random 100)))
 (dotimes (i 400)
   (lcos (format nil "qline 0 ~a ~a 200" a i))
   (lcos (format nil "qnumber ~a" i))
   (lcos "qswap")))

(defparameter *lcos-chan2* nil)
(defun lco2 (cmd)
  (let ((s (sb-ext:process-input *lcos-chan2*)))
    (format s "~a~%" cmd)
    (finish-output s)))
(progn
  (sb-posix:setenv "DISPLAY" ":0" 1)
  (setf *lcos-chan2*
        (sb-ext:run-program "/home/martin/0505/mma/glfw-server/glfw" '("400" "400")
                            :output :stream
                            :input :stream
                            :wait nil))
  
  (sb-thread:make-thread 
   #'(lambda ()
       (unwind-protect
           (with-open-stream (s (sb-ext:process-output *lcos-chan2*))
             (loop for line = (read-line s nil nil)
                while line do
                  (format t "lco2 read: ~a~%" line)
                  (finish-output)))
         (sb-ext:process-close *lcos-chan2*)))
   :name "cmd-reader2"))

#+nil
(let ((a (random 100)))
 (dotimes (i 400)
   (lco2 (format nil "qline 0 ~a ~a 200" a i))
   (lco2 (format nil "qnumber ~a" i))
   (lco2 "qswap")))
