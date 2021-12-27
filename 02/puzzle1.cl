(defvar horizontal 0)
(defvar depth 0)

(defun forward (n) (setf horizontal (+ horizontal n)))

(defun down (n) (setf depth (+ depth n)))
(defun up (n) (down (- n)))
(load "input.cl")

; multiply your final horizontal position by your final depth

(defconstant answer (* horizontal depth))
(princ answer)

(exit)
