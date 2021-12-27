(defvar aim 0)
(defvar horizontal 0)
(defvar depth 0)

(defun forward (n)
   (setf horizontal (+ horizontal n))
   (setf depth (+ depth (* aim n)))
)

(defun down (n) (setf aim (+ aim n)))
(defun up (n) (down (- n)))
(load "input.cl")

(defconstant answer (* horizontal depth))
(princ answer)

(exit)
