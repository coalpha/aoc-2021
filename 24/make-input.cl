(load "../shared/common.cl")

(defconstant ifd (open "input.txt"))
(defconstant ofd (open "input.cl"
   :direction :output
   :if-exists :supersede
   :if-does-not-exist :create
))

(write-line "(defun 24-prog (24-input &optional (w 0) (x 0) (y 0) (z 0))" ofd)
(defvar *line*)
(let (line)
   (loop
      (setf line (read-line ifd nil))
      (if (null line) (return))
      (write-line (strcat@ "   (24-" line ")") ofd)))

(write-line "   (list w x y z)" ofd)
(write-line ")" ofd)
(finish-output ofd)
(exit)
