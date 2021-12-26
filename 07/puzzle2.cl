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

(defun cost (x)
   (defun distance (e) (abs (- e x)))
   (defun fuel-cost (D) (* D (+ D 1) 1/2))
   (defun elementwise (e) (fuel-cost (distance e)))
   (apply '+ (mapcar 'elementwise crabs)))

(defconstant minx (/ (apply '+ crabs) (length crabs)))
(defconstant minf (floor minx))
(defconstant minc (ceiling minx))

(defconstant costf (cost minf))
(defconstant costc (cost minc))

(defconstant posx! (if (< costf costc) minf minc))
(defconstant cost! (if (< costf costc) costf costc))

(format t "best-x = ~D~%" minx)
(format t "cost(x=~D) = ~D~%" posx! cost!)
(exit)
