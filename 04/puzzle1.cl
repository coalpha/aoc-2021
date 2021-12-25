(defun read-lines (fd)
   (let ((line (read-line fd nil)))
      (and line (append (list line) (read-lines fd)))))

(defconstant fd (open "input.txt"))
(defconstant lines (read-lines fd))
(defconstant lines-length (length lines))

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

(defvar *marked* nil)
(defvar *to-be-marked*
   (mapcar 'parse-integer (string-split #\, (car lines))))

(defmacro strcat (&rest strs)
   `(concatenate 'string ,@strs))

(defun read-board (rows)
   (assert (= 6 (length rows)))
   (read-from-string (strcat
      "("
      "(" (elt rows 1) ")"
      "(" (elt rows 2) ")"
      "(" (elt rows 3) ")"
      "(" (elt rows 4) ")"
      "(" (elt rows 5) ")"
      ")"
   )))

(defun is-marked (num)
   (member num *marked*))

(defun print-digit (d)
   (if (is-marked d)
      (format t "~2D! " d)
      (format t "~2D  " d)))

(defun print-row (row)
   (format t "│ ")
   (mapc 'print-digit row)
   (format t "│~%"))

(defun print-board (board)
   (format t "┌─────────────────────┐~%")
   (mapc 'print-row board)
   (format t "└─────────────────────┘~%")
   (terpri))

(defun col (board col)
   (mapcar (lambda (row) (elt row col)) board))

(defun board-has-won (board) (or
   (every 'is-marked (elt board 0))
   (every 'is-marked (elt board 1))
   (every 'is-marked (elt board 2))
   (every 'is-marked (elt board 3))
   (every 'is-marked (elt board 4))

   (every 'is-marked (col board 0))
   (every 'is-marked (col board 1))
   (every 'is-marked (col board 2))
   (every 'is-marked (col board 3))
   (every 'is-marked (col board 4))))

(defvar *boards* nil)
(do ((i 1 (+ i 6)) board) ((>= i lines-length) nil)
   (setf board (read-board (subseq lines i (+ i 6))))
   (setf *boards* (append *boards* (list board))))

(defvar *marking* nil)
(defvar *winning-board* nil)
(loop
   (setf *marking* (car *to-be-marked*))
   (setf *to-be-marked* (cdr *to-be-marked*))
   (setf *marked* (append *marked* (list *marking*)))
   (format t "Calling ~D~%" *marking*)
   (mapc 'print-board *boards*)
   (setf *winning-board* (find-if 'board-has-won *boards*))
   (if *winning-board*
      (return))
)

(format t "A board has won!~%")

(defun board-flatten (board)
   (apply 'append (mapcar (lambda (vec) (coerce vec 'list)) board)))

(defconstant sum-of-unmarked
   (apply '+ (remove-if 'is-marked (board-flatten *winning-board*))))

(format t "The final score is ~D~%" (* sum-of-unmarked *marking*))
(exit)
