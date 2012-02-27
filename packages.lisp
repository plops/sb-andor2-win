(defpackage :sb-andor2-win-internal
  (:nicknames :and)
  (:use :cl :sb-alien)
  (:export))

(defpackage :sb-andor2-win
  (:export)
  (:use :cl :sb-andor2-win-internal))