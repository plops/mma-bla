(in-package :qng)
;; port of gsl-1.14/integration/qng.{c,h}
(declaim (optimize (speed 2) (debug 3) (safety 3)))

(defmacro defa (name &rest numbers)
  `(progn
     (defparameter ,name 
       ,(make-array (length numbers)
		   :element-type 'single-float
		   :initial-contents (mapcar 
				      #'(lambda (q) (coerce q 'single-float))
				      numbers)))
     (declaim ((simple-array single-float 1) ,name))))

(defa x1
    0.973906528517171720077964012084452
  0.865063366688984510732096688423493
  0.679409568299024406234327365114874
  0.433395394129247190799265943165784
  0.148874338981631210884826001129720)

(defa w10
    0.066671344308688137593568809893332
  0.149451349150580593145776339657697
  0.219086362515982043995534934228163
  0.269266719309996355091226921569469
  0.295524224714752870173892994651338)

(defa x2
    0.995657163025808080735527280689003
  0.930157491355708226001207180059508
  0.780817726586416897063717578345042
  0.562757134668604683339000099272694
  0.294392862701460198131126603103866)

(defa w21a
    0.032558162307964727478818972459390
  0.075039674810919952767043140916190
  0.109387158802297641899210590325805
  0.134709217311473325928054001771707
  0.147739104901338491374841515972068)

(defa w21b
    0.011694638867371874278064396062192
  0.054755896574351996031381300244580
  0.093125454583697605535065465083366
  0.123491976262065851077958109831074
  0.142775938577060080797094273138717
  0.149445554002916905664936468389821)

(defa x3
    0.999333360901932081394099323919911
  0.987433402908088869795961478381209
  0.954807934814266299257919200290473
  0.900148695748328293625099494069092
  0.825198314983114150847066732588520
  0.732148388989304982612354848755461
  0.622847970537725238641159120344323
  0.499479574071056499952214885499755
  0.364901661346580768043989548502644
  0.222254919776601296498260928066212
  0.074650617461383322043914435796506)

(defa w43a
    0.016296734289666564924281974617663
  0.037522876120869501461613795898115
  0.054694902058255442147212685465005
  0.067355414609478086075553166302174
  0.073870199632393953432140695251367
  0.005768556059769796184184327908655
  0.027371890593248842081276069289151
  0.046560826910428830743339154433824
  0.061744995201442564496240336030883
  0.071387267268693397768559114425516)

(defa w43b
  0.001844477640212414100389106552965
  0.010798689585891651740465406741293
  0.021895363867795428102523123075149
  0.032597463975345689443882222526137
  0.042163137935191811847627924327955
  0.050741939600184577780189020092084
  0.058379395542619248375475369330206
  0.064746404951445885544689259517511
  0.069566197912356484528633315038405
  0.072824441471833208150939535192842
  0.074507751014175118273571813842889
  0.074722147517403005594425168280423)

(defa x4
   0.999902977262729234490529830591582
  0.997989895986678745427496322365960
  0.992175497860687222808523352251425
  0.981358163572712773571916941623894
  0.965057623858384619128284110607926
  0.943167613133670596816416634507426
  0.915806414685507209591826430720050
  0.883221657771316501372117548744163
  0.845710748462415666605902011504855
  0.803557658035230982788739474980964
  0.757005730685495558328942793432020
  0.706273209787321819824094274740840
  0.651589466501177922534422205016736
  0.593223374057961088875273770349144
  0.531493605970831932285268948562671
  0.466763623042022844871966781659270
  0.399424847859218804732101665817923
  0.329874877106188288265053371824597
  0.258503559202161551802280975429025
  0.185695396568346652015917141167606
  0.111842213179907468172398359241362
  0.037352123394619870814998165437704)

(defa w87a
    0.008148377384149172900002878448190
  0.018761438201562822243935059003794
  0.027347451050052286161582829741283
  0.033677707311637930046581056957588
  0.036935099820427907614589586742499
  0.002884872430211530501334156248695
  0.013685946022712701888950035273128
  0.023280413502888311123409291030404
  0.030872497611713358675466394126442
  0.035693633639418770719351355457044
  0.000915283345202241360843392549948
  0.005399280219300471367738743391053
  0.010947679601118931134327826856808
  0.016298731696787335262665703223280
  0.021081568889203835112433060188190
  0.025370969769253827243467999831710
  0.029189697756475752501446154084920
  0.032373202467202789685788194889595
  0.034783098950365142750781997949596
  0.036412220731351787562801163687577
  0.037253875503047708539592001191226)

(defa w87b
    0.000274145563762072350016527092881
  0.001807124155057942948341311753254
  0.004096869282759164864458070683480
  0.006758290051847378699816577897424
  0.009549957672201646536053581325377
  0.012329447652244853694626639963780
  0.015010447346388952376697286041943
  0.017548967986243191099665352925900
  0.019938037786440888202278192730714
  0.022194935961012286796332102959499
  0.024339147126000805470360647041454
  0.026374505414839207241503786552615
  0.028286910788771200659968002987960
  0.030052581128092695322521110347341
  0.031646751371439929404586051078883
  0.033050413419978503290785944862689
  0.034255099704226061787082821046821
  0.035262412660156681033782717998428
  0.036076989622888701185500318003895
  0.036698604498456094498018047441094
  0.037120549269832576114119958413599
  0.037334228751935040321235449094698
  0.037361073762679023410321241766599)

(defconstant single-float-min 1.1754943508222875e-38)

(declaim (single-float single-float-min))

(defun rescale-error (err result-abs result-asc)
  (declare (single-float err result-abs result-asc)
	   (values single-float &optional))
  (setf err (abs err))
  (if (and (/= result-asc 0s0)
	   (/= err 0s0))
      (let ((scale (expt (/ (* 200s0 err) result-asc) 1.5s0)))
	(if (< scale 1)
	    (setf err (* scale result-asc))
	    (setf err result-asc)))
      (if (< (/ single-float-min (* 50s0 single-float-epsilon)) result-abs)
	  (let ((min-err (* 50s0 single-float-epsilon result-abs)))
	    (when (< err min-err)
	      (setf err min-err)))))
  err)
#+nil
(rescale-error .1 1.0 2.0)


(defun qng (fun a b &key (epsabs 1s0) (epsrel 1s-2))
  "This function applies the Gauss-Kronrod 10-point, 21-point,
43-point and 87-point integration rules in succession until an
estimate of the integral of f over (a,b) is achieved within the
desired absolute and relative error limits, epsabs and epsrel. The
function returns the final approximation, result, an estimate of the
absolute error, abserr and the number of function evaluations used,
neval. The Gauss-Kronrod rules are designed in such a way that each
rule uses all the results of its predecessors, in order to minimize
the total number of function evaluations. The programs tries to
calculate a result that fullfills |result-I|<=max(epsabs,epsrel*|I|)
and the estimated abserr should fullfill
|result-I|<=abserr<=max(epsabs,epsrel*|I|)."
  (declare (function fun)
	   (single-float a b epsabs epsrel)
	   (values single-float  ;; result
		   single-float  ;; abserr
		   fixnum ;; neval
		   &optional))
  (let* ((fv1 (make-array 5 :element-type 'single-float))
	 (fv2 (make-array 5 :element-type 'single-float))
	 (fv3 (make-array 5 :element-type 'single-float))
	 (fv4 (make-array 5 :element-type 'single-float))
	 (savfun (make-array 21 :element-type 'single-float)) ;; computed function vals
	 (res10 0s0) (res21 0s0) (res43 0s0) (res87 0s0) ;; 10..87 point results
	 (result-kronrod 0s0) (err 0s0)
	 (resabs 0s0) ;; aproximation of \int|f|
	 (resasc 0s0) ;; approximation of \int|f-Integral/(b-a)|
	 (half-length (* .5s0 (- b a)))
	 (abs-half-length (abs half-length))
	 (center (* .5s0 (+ b a)))
	 (f-center (funcall fun center)))
    (when (and (<= epsabs 0) 
	       (or (< epsrel (* 50 single-float-epsilon))
		   (< epsrel .5e-14)))
      (error "tolerance cannot be achieved with given epsabs and epsrel."))
    ;; compute integral using 10- and 21-point formula 
    (setf res10 0s0
	  res21 (* f-center (aref w21b 5))
	  resabs (* (abs f-center) (aref w21b 5)))
    (dotimes (k 5)
      (let* ((abscissa (* half-length (aref x1 k)))
	     (fval1 (funcall fun (+ center abscissa)))
	     (fval2 (funcall fun (- center abscissa)))
	     (fval (+ fval1 fval2)))
	(incf res10 (* (aref w10 k) fval))
	(incf res21 (* (aref w21a k) fval))
	(incf resabs (* (aref w21a k) (+ (abs fval1) (abs fval2))))
	(setf (aref savfun k) fval
	      (aref fv1 k) fval1
	      (aref fv2 k) fval2)))
    (dotimes (k 5)
      (let* ((abscissa (* half-length (aref x2 k)))
	     (fval1 (funcall fun (+ center abscissa)))
	     (fval2 (funcall fun (- center abscissa)))
	     (fval (+ fval1 fval2)))
	(incf res21 (* (aref w21b k) fval))
	(incf resabs (* (aref w21b k) (+ (abs fval1) (abs fval2))))
	(setf (aref savfun (+ 5 k)) fval
	      (aref fv3 k) fval1
	      (aref fv4 k) fval2)))
    (setf resabs (* resabs abs-half-length))
    (let ((mean (* .5s0 res21)))
      (setf resasc (* (aref w21b 5) (abs (- f-center mean))))
      (dotimes (k 5)
	(incf resasc (+ (* (aref w21a k) (+ (abs (- (aref fv1 k) mean))
					    (abs (- (aref fv2 k) mean))))
			(* (aref w21b k) (+ (abs (- (aref fv3 k) mean))
					    (abs (- (aref fv4 k) mean)))))))
      (setf resasc (* resasc abs-half-length)))
    (setf result-kronrod (* res21 half-length)
	  err (rescale-error (* half-length (- res21 res10))
			     resabs
			     resasc))
    ;; test for convergence
    (when (or (< err epsabs)
	      (< err (* epsrel (abs result-kronrod))))
      (return-from qng (values result-kronrod err 21)))
    ;; compute 43-point formula
    (setf res43 (* (aref w43b 11) f-center))
    (dotimes (k 10)
      (incf res43 (* (aref savfun k) (aref w43a k))))
    (dotimes (k 11)
      (let* ((abscissa (* (aref x3 k) half-length))
	     (fval (+ (funcall fun (+ center abscissa))
		      (funcall fun (- center abscissa)))))
	(incf res43 (* fval (aref w43b k)))
	(setf (aref savfun (+ 10 k)) fval)))
    ;; test for convergence
    (setf result-kronrod (* res43 half-length)
	  err (rescale-error (* half-length (- res43 res21))
			     resabs
			     resasc))
    (when (or (< err epsabs)
	      (< err (* epsrel (abs result-kronrod))))
      (return-from qng (values result-kronrod err 43)))
    ;; compute 87-point formula
    (setf res87 (* (aref w87b 22) f-center))
    (dotimes (k 21)
      (incf res87 (* (aref savfun k) (aref w87a k))))
    (dotimes (k 22)
      (let* ((abscissa (* (aref x4 k) half-length)))
	(incf res87 (* (aref w87b k) (+ (funcall fun (+ center abscissa))
					(funcall fun (- center abscissa)))))))
    ;; test for convergence
    (setf result-kronrod (* res87 half-length)
	  err (rescale-error (* half-length (- res87 res43))
			     resabs
			     resasc))
    (when (or (< err epsabs)
	      (< err (* epsrel (abs result-kronrod))))
      (return-from qng (values result-kronrod err 87)))
    (error "failed to reach tolerance with highest-order rule.")))

#+nil
(multiple-value-bind (result abserr neval)
    (qng #'(lambda (q) (* (sqrt q) (log q))) 0s0 1s0 :epsabs .01 :epsrel .01)
  (format nil "~a~%" (list 'true-error (- -4/9 result) 'error-estimate abserr
			   'evals neval)))