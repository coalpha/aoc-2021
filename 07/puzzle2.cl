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

(defconstant crabs
   (mapcar 'parse-integer
      (string-split #\, (read-line fd))))

(defconstant guess (round (/ (apply '+ crabs) (length crabs))))

(defun cost (x)
   (defun distance (e) (abs (- e x)))
   (defun fuel-cost (D) (* D (+ D 1) 1/2))
   (defun elementwise (e) (fuel-cost (distance e)))
   (apply '+ (mapcar 'elementwise crabs)))

(defvar *minimum-cost* nil)
(defvar *minimum-posi* nil)
(defvar *current-cost* nil)
(defun less-cost? (x)
   (or (null *minimum-cost*) (< x *minimum-cost*)))

(defconstant imin (apply 'min crabs))
(defconstant imax (apply 'max crabs))
(loop for x from imin to imax do
   (setf *current-cost* (cost x))
   (if (less-cost? *current-cost*)
      (progn
         (setf *minimum-posi* x)
         (setf *minimum-cost* *current-cost*))))

(format t "guess : minimum-cost at x=~D~%" guess)
(format t "actual: minimum-cost ~D at x=~D~%" *minimum-cost* *minimum-posi*)
(exit)
