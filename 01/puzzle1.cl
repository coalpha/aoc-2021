(defconstant input_fd (open "input.txt"))

(defun read-int ()
   (let ((line (read-line input_fd nil)))
      (and line (parse-integer line))))

(let ((snd (read-int)) fst (i 0))
   (loop
      (setf fst snd)
      (setf snd (read-int))
      (if (null snd)
         (return))
      (if (> snd fst)
         (setf i (+ i 1)))
   )
   (princ i)
)
(exit)
