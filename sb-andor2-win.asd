(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-grovel))

(defpackage #:sb-andor2-win.system
  (:use #:asdf #:cl))
(in-package #:sb-andor2-win.system)

(defsystem :sb-andor2-win
  :depends-on (sb-grovel)
  :components ((:file "packages")
	       (:file "internal" :depends-on ("packages"))
	       (sb-grovel:grovel-constants-file "constants"
				      :package :sb-andor2-win-internal
				      :depends-on ("internal" "packages"))))