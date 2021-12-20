(defconstant input_fd (open "input.cl"))
(defvar horizontal 0)
(defvar depth 0)

(defun forward (n) (setf horizontal (+ horizontal n)))

(defun down (n) (setf depth (+ depth n)))
(defun up (n) (down (- n)))
(defvar line)
(loop
   (setf line (read-line input_fd nil))
   (print line)
   (if (null line)
      (return))
   (setf line (car (read-from-string line)))
   (print line)
   (eval line))

; multiply your final horizontal position by your final depth

(defconstant answer (* horizontal depth))
(princ answer)

(close input_fd)
(exit)
