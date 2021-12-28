(load "../shared/common.cl")
(defconstant fd (open "input.test.txt"))

(defvar *width* 0)
(defvar *height* 0)
(defvar *cqkmbr* nil)

(defun init-cqkmbr ()
   (defun cqkmbr-list (first-line? &optional (chr (read-char fd nil)) sym)
      (if (null chr)
         (return-from cqkmbr-list nil))

      (ifn (eq chr #\newline)
         (inc *height*)
         (return-from cqkmbr-list (cqkmbr-list nil)))

      (setf sym (cond
         ((eq chr #\>) :>)
         ((eq chr #\v) :v)
         ((eq chr #\.) :.)
         (t (error "'~C' is not one of the expected characters!~%" chr))))

      (if first-line?
         (inc *width*))
      (setf *cqkmbr* (append *cqkmbr* (list sym)))

      (cqkmbr-list first-line?))

   (cqkmbr-list t)

   (setf *cqkmbr* (apply 'vector *cqkmbr*))
)

(defun i->x (i) (mod i *width*))
(defun i->y (i) (i/ i *width*))

(defun xy->i (x y)
   (+ x (* y *width*)))

(defun x-correct (x) (if (= x *width*) 0 x))
(defun y-correct (y) (if (= y *height*) 0 y))

(defun print-cqkmbr ()
   (princ "┌")
   (dotimes (n *width*)
      (princ "─"))
   (format t "┐~%")
   (dotimes (y *height*)
      (princ "│")
      (dotimes (x *width*)
         (princ (aref *cqkmbr* (xy->i x y))))
      (format t "│~%"))

   (princ "└")
   (dotimes (n *width*)
      (princ "─"))
   (format t "┘~%"))

(init-cqkmbr)
