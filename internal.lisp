(in-package :sb-andor2-win-internal)

(defun lookup-error (num)
  (gethash num *drv-hash*))

(defun check (num)
  (unless (= num DRV_SUCCESS)
    (break "error: ~d ~a" num (lookup-error num))))

#+nil
(check
 (initialize* (sb-sys:int-sap 0)))
#+nil
(with-alien ((c (struct andorcaps)))
  (let ((p (addr c)))
    (setf (andorcaps-size p) size-of-andorcaps)
    (get-capabilities* p)
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
	 'trigger (andorcaps-trigger-modes p))))
