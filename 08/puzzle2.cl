; segment -> digit
(defconstant ALLDIGITS '(0 1 2 3 4 5 6 7 8 9))
(defconstant A '(0 2 3 5 6 7 8 9))
(defconstant B '(0 4 5 6 8 9))
(defconstant C '(0 1 2 3 4 7 8 9))
(defconstant D '(2 3 4 5 6 8 9))
(defconstant E '(0 2 6 8))
(defconstant F '(0 1 3 4 5 6 7 8 9))
(defconstant G '(0 2 3 5 6 8 9))

; wire -> digit
(defconstant d A)
(defconstant e B)
(defconstant a C)
(defconstant f D)
(defconstant g E)
(defconstant b F)
(defconstant c G)

(defconstant fd (open "input.test.txt"))

; given xxx xxx xxx | yyy yyy
; returns '(yyy yyy)
(defun get-display (&optional (line (read-line fd nil)))
   (or line (cdr (words (cadr (string-split #\| line))))))

(defun get-var (str) (eval (read-from-string str)))

(defun word-to-digit (word &optional (ret ALLDIGITS))
   (defun elementwise (chr)
      (setf ret (intersection ret (get-var (string chr)))))
   (foreach* 'elementwise word)
   ret)

