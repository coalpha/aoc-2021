(defun read-lines (fd)
   (let ((line (read-line fd nil)))
      (and line (append (list line) (read-lines fd)))))

(defconstant fd (open "input.test.txt"))
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

(defmacro strcat (&rest strs)
   `(concatenate 'string ,@strs))

(defun parse-coordinate (str)
   (read-from-string (strcat "(" @(string-split #\, str) ")")))

(defstruct line-segment start end)

(defun parse-line (line)
   (let ((words (string-split #\ line)))
      (make-line-segment
         :start (parse-coordinate (elt words 0))
         :end   (parse-coordinate (elt words 2)))))

(defconstant max-x 99)
(defconstant max-y 99)
(defconstant max-i (* max-x max-y))
(defconstant field (make-array max-i :element-type 'integer :initial-element 0))
(defmacro xyi (x y) (+ x (* y max-x)))

(defun horizontalp (lsgmt) (line-segment-start))
