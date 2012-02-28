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
	    ('single-scan 1)
	    ('accumulate 2)
	    ('kinetics 3)
	    ('fast-kinetics 4)
	    ('run-till-abort 5)))))

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