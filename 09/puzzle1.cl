(load "../shared/common.cl")

(defconstant fd (open "input.txt"))

(defvar *lines* (fd-lines fd))

(defun chr->int (c)
   (cond
      ((eq #\0 c) 0)
      ((eq #\1 c) 1)
      ((eq #\2 c) 2)
      ((eq #\3 c) 3)
      ((eq #\4 c) 4)
      ((eq #\5 c) 5)
      ((eq #\6 c) 6)
      ((eq #\7 c) 7)
      ((eq #\8 c) 8)
      ((eq #\9 c) 9)))

(defun get-xy (x y)
   (or (ignore-errors (chr->int (aref (elt *lines* y) x))) 9))

(defvar *total-risk* 0)

(list-for (y line yl) in *lines* do
   (vec-for (x chr xl) in line do
      (let (
         (here (chr->int chr))
         (w (get-xy x (- y 1)))
         (a (get-xy (- x 1) y))
         (s (get-xy x (+ y 1)))
         (d (get-xy (+ x 1) y)))
         (and
            (< here w)
            (< here a)
            (< here s)
            (< here d)
            (+= *total-risk* (+ 1 here))
         )
      )
   )
)

(princ *total-risk*)
(exit)
