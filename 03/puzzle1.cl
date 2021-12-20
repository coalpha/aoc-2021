(defun read-lines (fd)
   (let ((line (read-line fd nil)))
      (and line (append (list line) (read-lines fd)))))

(defconstant fd (open "input.txt"))
(defconstant file (read-lines fd))

(defun get-bit (n)
   (lambda (from) (ash (logand from (ash 1 n)) (- n))))

(defun set-bit (n on) (logior on (ash 1 n)))

(defmacro succ (sym) `(setf ,sym (+ ,sym 1)))

(defun most-common-bin (lst)
   (let ((zero 0) (one 0))
      (loop for n in lst do
         (if (= n 0) (succ zero) (succ one)))
      (if (< zero one) 1 0)
   )
)

(defconstant bit-length (length (car file)))
(defun from-bin (str) (parse-integer str :radix 2))

(defconstant bins (mapcar 'from-bin file))

(defvar gamma 0)
(defvar epsilon 0)
(do ((i 0 (+ i 1)) b) ((>= i bit-length) nil)
   (setf b (most-common-bin (mapcar (get-bit i) bins)))
   (if (= b 1)
      (setf gamma (set-bit i gamma))
      (setf epsilon (set-bit i epsilon))))

(defconstant power (* gamma epsilon))
(format t "gamma  : 0b~13B~%" gamma)
(format t "epsilon: 0b~13B~%" epsilon)
(format t "power  : 0d~D~%" power)
(exit)
