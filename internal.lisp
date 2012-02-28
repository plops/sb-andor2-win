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
(initialize)

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

(defun shutdown ()
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
    (check (set-trigger-mode* m)))))

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