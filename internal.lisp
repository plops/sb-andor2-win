(in-package :sb-andor2-win-internal)

(defun lookup-error (num)
  (gethash num *drv-hash*))

(defun check (num)
  (unless (= num DRV_SUCCESS)
    (break "error: ~d ~a" num (lookup-error num))))

(defun initialize ()
 (check
  (initialize* "/usr/local/etc/andor")))

#+nil
(is-cooler-on*)


#+nil
(time
 (initialize))

(defparameter *bla* nil)

#+nil
(progn ;; small
 (defparameter *w* 13 #+nil 92)
 (defparameter *h* 10 #+nil 40))
(defparameter *w* 1392)
(defparameter *h* 1040)

#+nil
(progn
  (defparameter *w* 512)
  (defparameter *h* 512))

#+nil
(get-detector)

#+nil
(progn ;; kinetics series 
  (set-acquisition-mode
    'run-till-abort
   #+nil 'single-scan 
   #+nil 'kinetics)
  (set-exposure-time .02)
  (check (set-number-accumulations* 1))
  ;; (check (set-accumulation-cycle-time* .2))
  (check (set-kinetic-cycle-time* .06f0))
  ;(check (set-number-kinetics* 19))
  (set-read-mode 'image)
  (set-vs-speed)
  ;; Note: there is a 10us gap in SHUTTER before FIRE starts
  (check (set-shutter* 1
		       0 ;; auto
		       0 1
		       )) 
  (check (set-frame-transfer-mode* 0))
  (set-fastest-hs-speed)
  (set-trigger-mode 'internal)
  (check (set-temperature* -40))
  (check (cooler-on*))
  (set-isolated-crop-mode* 0 *h* *w* 1 1)
  (set-image :xstart 1 :ystart 1 :xend *w* :yend *h*)
  (get-acquisition-timings))

#+nil 
(check ;; turn laser on constantly
 (set-shutter* 1 
	       1 ;; open 
	       0 1))

#+nil
(list (get-universal-time)
 (cadr (multiple-value-list (get-temperature-f*))))

#+nil
(get-size-of-circular-buffer*)

#+nil
(defun write-mma-disk (&key (cx 0) (cy 0) (r 1s0))
  ;; write a picture on mma
 (let* ((n 256)
	(nh (floor n 2))
	(buf (make-array (list n n) :element-type '(unsigned-byte 16)
			 :initial-element 0))
	(buf1 (sb-ext:array-storage-vector buf)))
   (dotimes (i n)
     (dotimes (j n)
       (let* ((x (* (+ i (- nh) cx) (/ 2s0 n)))
	      (y (* (+ j (- nh) cy) (/ 2s0 n)))
	      (rad (sqrt (+ (* x x) (* y y)))))
	 (setf (aref buf j i)
	       (if (<= rad r)
		  4095
		  0)))))
   (sb-sys:with-pinned-objects (buf)
     (mma::check
       (mma::write-matrix-data 1 3 (sb-sys:vector-sap buf1) 
			       (* n n))))))
#+nil
(write-mma-disk :cx 12 :cy 0 :r .2)
#+nil
(forthdd::forthdd-talk #x23 (list 34))
#+nil
(start-acquisition)
#+nil
(check
 (save-as-fits* "/dev/shm/o.fits" 0))
#+nil
(let ((buf (make-array (list *w* *h*)
		       :element-type '(unsigned-byte 16))))
  (sb-sys:with-pinned-objects (buf)
    (let* ((buf1 (sb-ext:array-storage-vector buf))
	   (buf-sap (sb-sys:vector-sap buf1)))
      (get-most-recent-image16* buf-sap (* *w* *h*))))
  (setf *bla* buf)
  nil)

#+nil
(loop for i from 0 below 10 do
     (write-mma-disk :cx (- (* 25 i) 128) :cy 0 :r .2)
     (forthdd::forthdd-talk #x23 (list i))
     (start-acquisition)
     (sleep .2)
     (save-as-fits* (format nil "/dev/shm/o~3,'0d.fits" i) 0))

#+nil
(abort-acquisition)
#+nil
(get-number-available-images*)
#+nil
(get-status)
#+nil
(check
 (free-internal-memory*))

;; 1125/camgui.py
(defun get-all-images16 (&key arr)
  "store all images from the circular buffer into ARR (an array which
is already allocated and can contain more data than needed)."
  (multiple-value-bind (num first last) (get-number-available-images*)
    (unless (= num DRV_SUCCESS)
      (break "error: get-number-available-images ~d ~a" num (lookup-error num)))
    (destructuring-bind (z y x) (array-dimensions arr)
      (let ((n (1+ (- last first))))
	(unless (< n z)
	  (break "warning: get-all-images16 needs storage for more images than supplied: ~a~%"
		 (list n z)))
	(let* ((arr1 (sb-ext:array-storage-vector arr))
	       (arr1-sap (sb-sys:vector-sap arr1)))
	 (multiple-value-bind (num valid-first valid-last) 
	     (get-images16* first last arr1-sap (* n y x))
	   (unless (= num DRV_SUCCESS)
	     (break "error: get-images16 ~d ~a" num (lookup-error num)))
	   (unless (and (= first valid-first)
			(= last valid-last))
	     (break "warning: get-images16 didn't return expected number of images ~a~%" 
		    (list first last valid-first valid-last)))
#+nil	   (check (free-internal-memory*))
	   (values n arr)))))))
#+nil
(setf sb-ext:*after-gc-hooks* 
      (list #'(lambda () (format t "gc ~a~%" (list (get-internal-real-time)))))) 
#+nil
(sb-ext:gc :full t)

(defun get-status ()
  (multiple-value-bind (err stat) (get-status*)
    (unless (= err DRV_SUCCESS)
      (break "error: ~d ~a" err (lookup-error err)))
    (lookup-error stat)))

#+nil
(get-status)

(defun abort-acquisition ()
  (when (eq 'drv_acquiring (get-status))
    (check (abort-acquisition*))))

(defun get-capabilities ()
 (with-alien ((c (struct andorcaps)))
   (let ((p (addr c)))
     (setf (andorcaps-size p) size-of-andorcaps)
     (check (get-capabilities* p))
     (list 'camera-type (andorcaps-camera-type p)
	   'features (andorcaps-features p)
	   'acq-modes (andorcaps-acq-modes p)
	   'emgain (andorcaps-emgain-capability p)
	   'ft-read (andorcaps-ft-read-modes p)
	   'pci (andorcaps-pci-card p)
	   'read (andorcaps-read-modes p)
	   'size (andorcaps-size p)
	   'get (andorcaps-get-functions p)
	   'set (andorcaps-set-functions p)
	   'pixel-mode (andorcaps-pixel-mode p)
	   'trigger (andorcaps-trigger-modes p)))))

(defun get-head-model ()
 (let* ((s (make-array 32 :element-type '(unsigned-byte 8))))
   (sb-sys:with-pinned-objects (s)
     (check (get-head-model* (sb-sys:vector-sap s))))
   ;; convert result into lisp string of sufficient length
   (map '(simple-array character 1) #'code-char
	(subseq s 0 (position 0 s)))))

#+nil
(get-head-model)

(defun get-detector ()
 (with-alien ((w int)
	      (h int))
   (check (get-detector* (addr w) (addr h)))
   (list h w)))

#+nil
(get-detector)

(defun set-acquisition-mode (&optional (mode 'single-scan))
  (check (set-acquisition-mode* 
	  (ecase mode
	    (single-scan 1)
	    (accumulate 2)
	    (kinetics 3)
	    (fast-kinetics 4)
	    (run-till-abort 5)))))

#+nil 
(set-acquisition-mode)

(defun set-read-mode (&optional (mode 'image))
  (check (set-read-mode* 
	  (ecase mode
	    ('full-vertical-binning 0)
	    ('muli-track 1)
	    ('random-track 2)
	    ('single-track 3)
	    ('image 4)))))

#+nil
(set-read-mode)

(defun set-vs-speed (&optional index)
  (with-alien ((ind int)
	       (speed float))
    (if index
       (progn
	 (check (get-number-vs-speeds* (addr ind)))
	 (unless (< index ind)
	   (break "warning: index ~d too big. Continue with ~d." index (1- ind))
	   (setf index (1- ind)))
	 (check (get-vs-speed* index (addr speed)))
	 (check (set-vs-speed* index)))
       (progn
	 (check (get-fastest-recommended-vs-speed*
		 (addr ind) (addr speed)))
	 (check (set-vs-speed* ind))))
    speed))

#+nil
(set-vs-speed)

(defun set-fastest-hs-speed (&key em)
  (let ((conv-p (if em 0 1)))
    (let ((stemp 0s0)
	  (fast-ad 0)
	  (fast-ind 0))
      (with-alien ((channels int)
		   (ind int)
		   (speed float))
	(check (get-number-ad-channels* (addr channels)))
	(dotimes (c channels)
	  (check (get-number-hs-speeds* c conv-p (addr ind)))
	  (dotimes (i ind)
	    (check (get-hs-speed* c conv-p i (addr speed)))
	    (when (< stemp speed)
	      (setf stemp speed
		    fast-ad c
		    fast-ind i)))))
      (check (set-ad-channel* fast-ad))
      (check (set-hs-speed* conv-p fast-ind))
      stemp)))

#+nil
(set-fastest-hs-speed)

(defun set-baseline-clamp ()
  (let* ((caps (get-capabilities))
	 (base-p (/= 0 (logand (getf caps 'set) 
			       AC_SETFUNCTION_BASELINECLAMP))))
    (when base-p
      (check (set-baseline-clamp* 1)))))

#+nil
(set-baseline-clamp)

(defun shutdown () ;; for classic and iccd systems the temperature
  ;; should be above -20 degree before shutting down but I don't have
  ;; those
  (let* ((caps (get-capabilities))
	 (temp-p (/= 0 (logand (getf caps 'set) 
			       AC_SETFUNCTION_TEMPERATURE))))
    (when temp-p
      (check (cooler-off*)))
    (check (shut-down*))))

#+nil
(shut-down*)

(defun set-trigger-mode (&optional (mode 'internal))
  (let* ((m (ecase mode
	      (internal 0)
	      (external 1)
	      (external-start 6)
	      (bulb 7)
	      (external-fvb-em 9)
	      (software 10))))
    (when (= DRV_INVALID_MODE 
	     (is-trigger-mode-available* m))
      (break "trigger mode ~a isn't supported." mode))
    (check (set-trigger-mode* m))))

#+nil
(set-trigger-mode)


(defun get-maximum-binning (&optional (mode 'image))
  (let ((m (ecase mode
	     ('full-vertical-binning 0)
	     ('muli-track 1)
	     ('random-track 2)
	     ('single-track 3)
	     ('image 4))))
    (with-alien ((w int)
		 (h int))
     (check (get-maximum-binning* m 0 (addr w)))
     (check (get-maximum-binning* m 1 (addr h)))
     (list h w))))

#+nil
(get-maximum-binning)

(defun get-maximum-exposure ()
  (with-alien ((exp_s float))
    (check (get-maximum-exposure* (addr exp_s)))
    exp_s))

#+nil
(get-maximum-exposure)

(defun get-acquisition-timings ()
  (with-alien ((exposure float)
	       (accumulate float)
	       (kinetic float))
    (check
     (get-acquisition-timings* (addr exposure)
			       (addr accumulate)
			       (addr kinetic)))
    (values exposure accumulate kinetic)))
#+nil
(get-acquisition-timings)

(defun set-exposure-time (time_s)
  (check (set-exposure-time* time_s))
  (get-acquisition-timings))

#+nil
(set-exposure-time .02)

(defun set-image (&key (bin-h 1) (bin-w bin-h)
		  xstart xend ystart yend)
  (destructuring-bind (hh ww) (get-detector)
   (destructuring-bind (bh bw)
       (get-maximum-binning 'image)
     (let ((bbw (min bw (max 1 bin-w)))
	   (bbh (min bh (max 1 bin-h))))
       (unless xstart (setf xstart 1))
       (unless xend (setf xend ww))
       (unless ystart (setf ystart 1))
       (unless yend (setf yend hh))
       ;; first pixel is 1 (for xstart)
       ;; xend can be at most ww 
       (check (set-image* bbw bbh 
			  xstart xend
			  ystart yend))))))

#+nil
(set-image)

#+nil
(set-image :xstart 1 :ystart 1 :xend 512 :yend 512)

(defun start-acquisition ()
  (check (start-acquisition*)))

#+nil
(progn
  (time
   (prepare-acquisition*))
  (time
   (progn
     (start-acquisition)
     (wait-for-acquisition*))))

(defun get-most-recent-image ()
 (destructuring-bind (h w) (list *w* *h*)
   (let ((a (make-array (list h w)
			:element-type '(unsigned-byte 16))))
     (sb-sys:with-pinned-objects (a)
       (check (get-most-recent-image16* 
	       (sb-sys:vector-sap 
		(sb-ext:array-storage-vector a))
	       (* h w))))
     a)))
