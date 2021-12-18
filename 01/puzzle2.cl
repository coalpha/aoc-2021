(defconstant input_fd (open "input.txt"))

(defun read-int ()
   (let ((line (read-line input_fd nil)))
      (and line (parse-integer line))))

(defun lt (a b) (and a b (< a b)))

(let (l0 l1 l2 l3 (i 0))
   (loop
      (setf l0 l1)
      (setf l1 l2)
      (setf l2 l3)
      (setf l3 (read-int))

      (if (null l3)
         (return))

      (if (lt l0 l3)
         (setf i (+ i 1)))
   )
   (princ i)
)

(exit)
