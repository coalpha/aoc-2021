(load "../shared/common.cl")

; NOTE: test input does not work
(defconstant fd (open "input.txt"))

(defconstant A '(0 2 3 5 6 7 8 9))
(defconstant B '(0 4 5 6 8 9))
(defconstant C '(0 1 2 3 4 7 8 9))
(defconstant D '(2 3 4 5 6 8 9))
(defconstant E '(0 2 6 8))
(defconstant F '(0 1 3 4 5 6 7 8 9))
(defconstant G '(0 2 3 5 6 8 9))

; given xxx xxx xxx | yyy yyy
; returns '(yyy yyy)
(defun parse-display (&optional (line (read-line fd nil)))
   (and line (cdr (words (cadr (string-split #\| line))))))

(defun word->digit (word)
   (setf word (str->set word))
   (cond
      ((set= word d0) 0)
      ((set= word d1) 1)
      ((set= word d2) 2)
      ((set= word d3) 3)
      ((set= word d4) 4)
      ((set= word d5) 5)
      ((set= word d6) 6)
      ((set= word d7) 7)
      ((set= word d8) 8)
      ((set= word d9) 9)
      (t (error "I don't know what ~F is!~%" word))))

(defun display->num (display)
   (+
      (* (word->digit (elt display 0)) 1000)
      (* (word->digit (elt display 1)) 0100)
      (* (word->digit (elt display 2)) 0010)
      (* (word->digit (elt display 3)) 0001)))

(let (display (acc 0))
   (loop
      (setf display (get-display))
      (if (null display)
         (return))
      (+= acc (display->num display)))
   (princ acc))
(exit)
