; After some careful analysis, the mapping between signal wires and segments
; only make sense in the following configuration:
;  dddd
; e    a
; e    a
;  ffff
; g    b
; g    b
;  cccc

(defun str->set (str) (coerce str 'list))
(defvar *digits* nil)
(defmacro d (n str)
   `(defconstant
      ,(intern (format nil "D~D" n))
      (list ,@(coerce str 'list))))

; So, the unique signal patterns would correspond to the following digits:

(d 0 "cagedb")
(d 1 "ab")
(d 2 "gcdfa")
(d 3 "fcadb")
(d 4 "eafb")
(d 5 "cdfeb")
(d 6 "cdfgeb")
(d 7 "dab")
(d 8 "acedgfb")
(d 9 "cefabd")

(load "../shared/common.cl")
; NOTE: test input does not work
(defconstant fd (open "input.txt"))

; given xxx xxx xxx | yyy yyy
; returns '(yyy yyy)
(defun get-display (&optional (line (read-line fd nil)))
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
