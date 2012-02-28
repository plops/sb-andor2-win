(in-package :sb-andor2-win-internal)

(defun lookup-error (num)
  (gethash num *drv-hash*))

(lookup-error
 (initialize* (sb-sys:int-sap 0)))