(load "shared.cl")

(defun 25-step (&optional (next (copy-seq *cqkmbr*)))
   (defun elementwise (i e &optional (x (i->x i)) (y (i->y i)) (nx x) (ny y) ni)
      (cond
         ((eq e :.) (return-from elementwise nil))
         ((eq e :v) (inc ny))
         ((eq e :>) (inc nx)))
      (setf nx (x-correct nx))
      (setf ny (y-correct ny))
      (setf ni (xy->i nx ny))
      (if (eq (aref next ni) :.)
         (rotatef (aref next i) (aref next ni))))

   (vec-forieach 'elementwise *cqkmbr*)

   next
)

(defvar *generations* 0)
(defun age (&optional (next (25-step))) "returns t on stable state"
   (inc *generations*)
   (format t "generation ~D~%" *generations*)
   (if (equal *cqkmbr* next)
      (progn (setf *cqkmbr* next) t)
      (progn (setf *cqkmbr* next) nil)))

(age)
(print-cqkmbr)
(exit)
; (loop
;    (if (age)
;       (return)))
