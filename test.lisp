(setf asdf:*central-registry* '("C:/Users/martin/Desktop/tmp/sb-andor2-win/"))
(asdf:oos 'asdf:compile-op :sb-andor2-win :verbose t)

(require :sb-andor2-win)