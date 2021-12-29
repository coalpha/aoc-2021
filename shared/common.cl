(defun fd-lines (fd)
   (let ((line (read-line fd nil)))
      (and line (append (list line) (fd-lines fd)))))

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

(defmacro += (sym what) `(setf ,sym (+ ,sym ,what)))
(defmacro inc (sym) `(+= ,sym 1))
(defmacro succ (i) `(+ ,i 1))
(defmacro strcat@ (&rest strs) `(concatenate 'string ,@strs))
(defmacro strcat (strs) `(apply 'concatenate '(append '('string) ,strs)))
(defmacro ifn (cond &rest form) `(if ,cond (progn ,@form)))
(defmacro unlessn (cond &rest form) `(ifn (not ,cond) ,@form))
(defmacro i/ (a b) `(floor (/ ,a ,b)))

(defun list-forieach (fn vec)
   (defun rec (i rest)
      (apply fn (car rest))
      (rec (succ i) (cdr rest)))
   (rec 0 vec))

(defun list-foreach (fn vec) (mapc fn vec) nil)

(defun vec-forieach (fn vec)
   (defun rec (i len)
      (unlessn (= i len)
         (apply fn (list i (aref vec i)))
         (rec (succ i) len))

      nil)

   (rec 0 (length vec)))

(defun vec-foreach (fn vec)
   (defun unary (e) (apply fn (list e)))
   (vec-forieach 'unary vec)
   nil)

(defun foreach* (fn any)
   (dolist (e (coerce any 'list))
      (apply fn (list e))))

(defun words (str) (string-split #\  str))
(defun set= (a b) (null (set-exclusive-or a b)))
(defun str->list (str) (coerce str 'list))
