(defconstant input_fd (open "input.txt" :direction :input))
(defconstant output_fd (open "input.cl"
   :direction :output
   :if-exists :supersede
   :if-does-not-exist :create
))

(defun write-expr (str)
   (write-line (concatenate 'string "(" str ")") output_fd))

(defvar line)
(loop
   (setf line (read-line input_fd nil))
   (if (null line)
      (return))
   (write-expr line))
(exit)
