(load "../shared/common.cl")

; NOTE: test input does not work
(defconstant fd (open "input.txt"))

(defun chars-words (what)
   (mapcar 'str->list (words what)))

(defun parse-line (line &optional barpos left right)
   (setf barpos (position #\| line))
   (setf left (subseq line 0 (- barpos 1)))
   (setf right (subseq line (+ barpos 2)))
   (list (chars-words left) (chars-words right)))

(defun size (n)
   (lambda (seq) (= (length seq) n)))

(defun subset-of (big)
   (lambda (small) (subsetp big small)))

(defmacro carsa (ary idx val)
   `(setf (aref ,ary ,idx) (car ,val)))

(defun has-element (e)
   (lambda (set) (member e set)))

(defun print-sln (A)
   (defun elementwise (i e)
      (format t "~7A: ~D~%" e i))
   (vec-forieach 'elementwise A))

(defun solve (Q &optional (A (make-array 10)) s5 d s6 e)
   (carsa A 1 (filter (size 2) Q))
   (carsa A 7 (filter (size 3) Q))
   (carsa A 4 (filter (size 4) Q))
   (carsa A 8 (filter (size 7) Q))

   (setf s5 (filter (size 5) Q)) ; {Q2 Q3 Q5}
   (carsa A 3 (filter (subset-of (aref A 1)) s5))
   (setf s5 (remove-if (subset-of (aref A 1)) s5))

   (setf d (car (intersection (aref A 4) (inner-join s5))))
   (setf s6 (filter (size 6) Q)) ; {Q6 Q9 Q0}
   (carsa A 6 (remove-if (subset-of (aref A 1)) s6))
   (setf s6 (filter (subset-of (aref A 1)) s6)) ; {Q9 Q0}
   (carsa A 9 (filter (has-element d) s6))
   (carsa A 0 (remove-if (has-element d) s6))

   (setf e (car (set-difference (aref A 8) (aref A 9))))
   (carsa A 5 (remove-if (has-element e) s5))
   (carsa A 2 (filter (has-element e) s5))
   A)

(defun set-lookup (vec set)
   (let ((i 0) (l (length vec)))
      (loop
         (if (= i l)
            (error "Cannot find ~F in ~F!~%" set vec))
         (if (set= set (aref vec i))
            (return)
            (inc i)))
      i
   )
)

(defvar *acc* 0)
(loop
   (let (line allten display A (sum 0))
      (setf line (read-line fd nil))
      (if (null line)
         (return))
      (set2f allten display (parse-line line))
      (setf A (solve allten))
      (setf sum (+
         (* (set-lookup A (elt display 0)) 1000)
         (* (set-lookup A (elt display 1)) 0100)
         (* (set-lookup A (elt display 2)) 0010)
         (* (set-lookup A (elt display 3)) 0001)))
      (format t "~A: ~D~%" display sum)
      (+= *acc* sum)))
(princ *acc*)
(exit)
