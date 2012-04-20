(in-package :sb-andor2-win-internal)

(defun lookup-error (num)
  (gethash num *drv-hash*))

(defun check (num)
  (unless (= num DRV_SUCCESS)
    (break "error: ~d ~a" num (lookup-error num))))

(defun initialize ()
 (check
  (initialize* (sb-sys:int-sap 0))))

#+nil
(time
 (initialize))

(defparameter *bla* nil)

(defun initialize-512 ()
  (initialize)
  (set-acquisition-mode)
  (set-read-mode)
  (set-vs-speed)
  (set-fastest-hs-speed)
  (set-trigger-mode)
  (set-exposure-time .01)
  (check (set-temperature* -5))
  (check (cooler-on*))
  (set-image :xstart 1 :ystart 1 :xend 512 :yend 512))

(let ((buf (make-array (list 42 512 512)
		       :element-type '(unsigned-byte 16))))
  (defun acquire-512 ()
   (start-acquisition)
   (check
    (wait-for-acquisition*)) ;; try wait-for-acquisition-time-out
   (format t "~a~%" (list (get-internal-real-time) 
			  (get-all-images16 :arr buf)))
   (setf *bla* buf)))

(defun do-something-while-idle ()
  (format t "~a "  (list (get-status) 
			 (multiple-value-bind (a b)
			     (get-temperature-f*) 
			   b))))

(let ((run-camera-p t))
  (defun stop-camera ()
      (setf run-camera-p nil))
  (defun camera-function ()
    (initialize-512)
    (loop while run-camera-p do
	 (do-something-while-idle)
	 (acquire-512))))

;(- 1931 647) ;; = 1284
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
	   (check (free-internal-memory*))
	   (values n arr)))))))

;; 560
#+nil
(sb-thread:make-thread #'camera-function :name "camera-thread")
#+nil
(push #'(lambda () (format t "gc ~a~%" (list (get-internal-real-time))))
      sb-ext:*after-gc-hooks*) 
#+nil
(sb-ext:gc :full t)
#+nil
(set-trigger-mode 'internal)

#+nil
(set-acquisition-mode 'single-scan)

#+nil
(start-acquisition)

#+nil
(get-temperature-f*)
#+nil
(check (set-temperature* -5))
#+nil
(check
 (cooler-on*))



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

#+nil
(abort-acquisition)
#+nil
(time
 (progn
  (start-acquisition)
  (check
   (wait-for-acquisition*))
  (check
   (abort-acquisition*))))

#+nil
(check
 (abort-acquisition*))

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

#+nil
(get-capabilities)


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
#+nil
(time
 (progn
   (start-acquisition)
   (wait-for-acquisition*)))
#+nil
(check (abort-acquisition*))

(defun get-most-recent-image ()
 (destructuring-bind (h w) (list 512 512)
   (let ((a (make-array (list h w)
			:element-type '(unsigned-byte 16))))
     (sb-sys:with-pinned-objects (a)
       (check (get-most-recent-image16* 
	       (sb-sys:vector-sap 
		(sb-ext:array-storage-vector a))
	       (* h w))))
     a)))

#+nil
(defparameter *bla*
 (get-most-recent-image))

#+nil
(dotimes (i 100)
  (start-acquisition)
  (sleep .2)
  (defparameter *bla*
    (get-most-recent-image)))


#+nil
(shutdown)
