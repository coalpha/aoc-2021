(load "../shared/common.cl")

(defmacro 24-inp (a)
   `(progn
      (if (null 24-input) (error "Cannot 24-inp! Input list is empty!~%"))
      (setf ,a (car 24-input))
      (setf 24-input (cdr 24-input))))
(defmacro 24-add (a b) `(setf ,a (+ ,a ,b)))
(defmacro 24-mul (a b) `(setf ,a (* ,a ,b)))
(defmacro 24-div (a b) `(setf ,a (truncate (/ ,a ,b))))
(defmacro 24-mod (a b) `(setf ,a (mod ,a ,b)))
(defmacro 24-eql (a b) `(setf ,a (if (= ,a ,b) 1 0)))

(load "input.cl")

(defun digits-of (n)
   (and (> n 0) (append (digits-of (floor (/ n 10))) (list (mod n 10)))))

(defun is-valid-sn (n) (= (elt (24-prog (digits-of n)) 3) 0))
