(defmacro += (sym what) `(setf ,sym (+ ,sym ,what)))
(defmacro inc (sym) `(+= ,sym 1))

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

(defconstant fd (open "input.txt"))
(defconstant days 80)
(defun make-state () (make-array 9 :element-type 'integer :initial-element 0))
(defvar *state* (make-state))

(dolist (intstr (string-split #\, (read-line fd)))
   (inc (aref *state* (parse-integer intstr))))

(defun next-state ()
   (let ((ret (make-state)))
      (+= (aref ret 7) (aref *state* 8))
      (+= (aref ret 6) (aref *state* 7))
      (+= (aref ret 5) (aref *state* 6))
      (+= (aref ret 4) (aref *state* 5))
      (+= (aref ret 3) (aref *state* 4))
      (+= (aref ret 2) (aref *state* 3))
      (+= (aref ret 1) (aref *state* 2))
      (+= (aref ret 0) (aref *state* 1))

      ; reproduction
      (+= (aref ret 6) (aref *state* 0))
      (+= (aref ret 8) (aref *state* 0))

      ret
   ))

(dotimes (day days)
   (setf *state* (next-state)))

(defconstant num-fishies (apply '+ (coerce *state* 'list)))

(format t "After ~D days, there are ~D fishies!~%" days num-fishies)

(exit)
