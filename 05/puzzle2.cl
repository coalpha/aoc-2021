(defmacro inc (sym) `(setf ,sym (+ ,sym 1)))
(defun read-lines (fd)
   (let ((line (read-line fd nil)))
      (and line (append (list line) (read-lines fd)))))

(defconstant fd (open "input.txt"))

(defun string-split (needle haystack)
   (if (= 0 (length haystack))
      (return-from string-split nil))

   (let ((idx (position needle haystack)))
      (if (null idx)
         (return-from string-split (list haystack)))

      (let (
         (word (subseq haystack 0 idx))
         (rest (subseq haystack (+ idx 1))))
            (append (list word) (string-split needle rest)))))

(defmacro strcat (&rest strs)
   `(concatenate 'string ,@strs))

(defun parse-coordinate (str)
   (let ((pair (string-split #\, str)))
      (assert (= 2 (length pair)))
      (read-from-string (strcat "(" (car pair) " " (cadr pair) ")"))))

(defun parse-line (line)
   (let ((words (string-split #\  line)))
      (list
         (parse-coordinate (elt words 0))
         (parse-coordinate (elt words 2)))))

(defconstant lines (mapcar 'parse-line (read-lines fd)))
(defconstant lines-length (length lines))

(defconstant A (car lines))

(defconstant max-x 1000)
(defconstant max-y 1000)
(defconstant max-i (* max-x max-y))
(defvar *field* (make-array max-i :element-type 'integer :initial-element 0))

(defmacro fref (x y) `(aref *field* (+ ,x (* ,y max-x))))

(defun print-field ()
   (loop for y below max-y
      do (loop for x below max-x
         do (format t "~D " (fref x y)))
         (format t "~%")))

(defun inc-field-ref (x y) (inc (fref x y)))

(defun horizontalp (lsgmt) (= (cadar lsgmt) (cadadr lsgmt)))
(defun verticalp (lsgmt) (= (caar lsgmt) (caadr lsgmt)))

(defun get-horizontal () "alright boys! let's get horizontal!"
   (remove-if-not 'horizontalp lines))

(defun get-vertical () "doesn't have quite the same ring to it, does it?"
   (remove-if-not 'verticalp lines))

(defun get-diagonal () "fuck go back"
   (remove-if 'verticalp (remove-if 'horizontalp lines)))

(defconstant horizontal (get-horizontal))
(defconstant vertical (get-vertical))
(defconstant diagonal (get-diagonal))

(defmacro lup (&let let &from from &to to &with sym &do what)
   `(let ,let
      (loop for ,sym from (min ,from ,to) to (max ,from ,to) do ,what)))

(format t "horzontal:~%~F~%" horizontal)
(dolist (lsgmt horizontal)
   (lup
      :let  ((y (cadar lsgmt)))
      :from (caar lsgmt)
      :to   (caadr lsgmt)
      :with x
      :do   (inc-field-ref x y)))

(format t "vertical:~%~F~%" vertical)
(dolist (lsgmt vertical)
   (lup
      :let  ((x (caar lsgmt)))
      :from (cadar lsgmt)
      :to   (cadadr lsgmt)
      :with y
      :do   (inc-field-ref x y)))

(defun sign (x) (
   cond
      ((> x 0) 1)
      ((> 0 x) -1)
      (t 0)))

; assuming |dy/dx| = 1
(format t "diagonal~%")
(dolist (lsgmt diagonal)
   (let* (
      (x0 (caar lsgmt)) (y0 (cadar lsgmt))
      (x1 (caadr lsgmt)) (y1 (cadadr lsgmt))
      (dx (sign (- x1 x0))) (dy (sign (- y1 y0)))
      (x x0) (y y0))
         (format t "~F ~3D, ~3D -> ~3D, ~3D: dx=~2D,dy=~2D~%" lsgmt x0 y0 x1 y1 dx dy)
         (loop
            (inc-field-ref x y)
            (if (= x x1)
               (return))
            (setf x (+ x dx))
            (setf y (+ y dy)))))

(defvar count>1 0)
(loop for i below max-i
   do (if (<= 2 (aref *field* i)) (inc count>1)))

(princ count>1)
(terpri)
(exit)
