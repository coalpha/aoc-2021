(defun read-lines (fd)
   (let ((line (read-line fd nil)))
      (and line (append (list line) (read-lines fd)))))

(defconstant fd (open "input.txt"))
(defconstant file (read-lines fd))
(defconstant bit-length (length (car file)))
(defconstant bins (mapcar (lambda (line) (parse-integer line :radix 2)) file))

(defun get-bit (n from)
   (ash (logand from (ash 1 n)) (- n)))

; most common bit n in the list
(defun bit-n-avg (n lst)
   (let ((zero 0) (one 0))
      (loop for e in lst do
         (if (= (get-bit n e) 0) (setf zero (+ 1 zero)) (setf one (+ 1 one))))
      (if (< one zero) 0 1)))

(defun bit-n-eq-to (n what)
   (lambda (from) (= (get-bit n from) what)))

(defmacro keep-if (&rest args)
   `(remove-if-not ,@args))

(defun print-bin-list (lst)
   (loop for e in lst do
      (format t "~12,'0,,B~%" e)))

(defvar oxygen)
(defvar oxygen-list bins)
(let ((i bit-length) common)
   (loop
      (format t "43210~%|||||~%")
      (print-bin-list oxygen-list)
      (if (= 1 (length oxygen-list))
         (progn
            (setf oxygen (car oxygen-list))
            (return)))
      (setf i (- i 1))
      (assert (>= i 0))
      (setf common (bit-n-avg i oxygen-list))
      (setf oxygen-list (keep-if (bit-n-eq-to i common) oxygen-list))
      (format t "keep common[~D] = ~D~%" i common)
   )
)

(defvar methane)
(defvar methane-list bins)
(let ((i bit-length) common)
   (loop
      (format t "43210~%|||||~%")
      (print-bin-list methane-list)
      (if (= 1 (length methane-list))
         (progn
            (setf methane (car methane-list))
            (return)))
      (setf i (- i 1))
      (assert (>= i 0))
      (setf common (bit-n-avg i methane-list))
      (setf methane-list (remove-if (bit-n-eq-to i common) methane-list))
      (format t "remove common[~D] = ~D~%" i common)
   )
)

(format t "oxygen : 0b~12,'0,,B~%" oxygen)
(format t "methane: 0b~12,'0,,B~%" methane)
(defconstant life-support-rating (* oxygen methane))
(format t "life-support-rating is ~D~%" life-support-rating)
(exit)
