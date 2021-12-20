(defconstant input_fd (open "input.cl"))
(defvar aim 0)
(defvar horizontal 0)
(defvar depth 0)

(defun forward (n)
   (setf horizontal (+ horizontal n))
   (setf depth (+ depth (* aim n)))
)

(defun down (n) (setf aim (+ aim n)))
(defun up (n) (down (- n)))
(defvar line)
(loop
   (setf line (read-line input_fd nil))
   (if (null line)
      (return))
   (setf line (read-from-string line))
   (eval line))

(defconstant answer (* horizontal depth))
(princ answer)

(close input_fd)
(exit)
