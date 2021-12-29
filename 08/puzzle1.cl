(load "../shared/common.cl")
(defconstant fd (open "input.txt"))

(defun is-1-7-4-8 (word &optional (l (length word)))
   (or
      (= l 2)
      (= l 3)
      (= l 4)
      (= l 7)))

(let ((count 0) line display words)
   (loop
      (setf line (read-line fd nil))
      (if (null line) (return))
      (setf display (cadr (string-split #\| line)))
      (setf words (cdr (string-split #\  display)))
      (dolist (w words)
         (if (is-1-7-4-8 w) (inc count))))
   (princ count))

(exit)
