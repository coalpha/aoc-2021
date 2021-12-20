(defconstant input_fd (open "input.txt" :direction :input))
(defconstant output_fd (open "input.cl"
   :direction :output
   :if-exists :supersede
   :if-does-not-exist :create
))

(defvar line)
(loop
   (setf line (read-line input_fd nil))
   (if (null line)
      (return))
   (setf line (concatenate 'string "(" line ")"))
   (princ line)
   (write-line line output_fd)
   (finish-output output_fd)
)
(exit)
