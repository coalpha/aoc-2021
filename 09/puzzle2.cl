(load "../shared/clike.cl")
(defconstant fd (open "input.txt"))
(defvar *lines* nil)

(clike
   let line = nil
   while line = read-line(fd nil) {
      psh!(*lines* line)
   }
)

(defun chr->int (c)
   (cond
      ((eq #\0 c) 0)
      ((eq #\1 c) 1)
      ((eq #\2 c) 2)
      ((eq #\3 c) 3)
      ((eq #\4 c) 4)
      ((eq #\5 c) 5)
      ((eq #\6 c) 6)
      ((eq #\7 c) 7)
      ((eq #\8 c) 8)
      ((eq #\9 c) 9)))

(defun val (pair)
   (or
      (ignore-errors (chr->int (aref (elt *lines* (cadr pair)) (car pair))))
      9
   )
)

(defvar *lows* nil)

(defun W@ (pair) `(,(car pair) ,(- (cadr pair) 1)))
(defun A@ (pair) `(,(- (car pair) 1) ,(cadr pair)))
(defun S@ (pair) `(,(car pair) ,(+ (cadr pair) 1)))
(defun D@ (pair) `(,(+ (car pair) 1) ,(cadr pair)))

(clike
   let y = 0
   let yl = length(*lines*)
   while <(y yl) {
      let x = 0
      let xl = length(elt(*lines* y))
      while <(x xl) {
         let c = [ x y ]
         let v = val(c)

         if and(
            <(v val(W@(c)))
            <(v val(A@(c)))
            <(v val(S@(c)))
            <(v val(D@(c)))
         ) { psh!(*lows* c) }

         x = +(x 1)
      }
      y = +(y 1)
   }
)

(defun has-seen (needle haystack)
   (defun pred (pt) (equal pt needle))
   (member-if 'pred haystack))

(defun basin (low)
   (clike
      let q = list(low)
      let visited = nil
      while q {
         let c = car!(q)
         let vc = val(c)

         if has-seen(c visited) {
            continue
         }

         psh!(visited c)
         let w = W@(c)
         let a = A@(c)
         let s = S@(c)
         let d = D@(c)

         let vw = val(w)
         let va = val(a)
         let vs = val(s)
         let vd = val(d)

         if and(<(vw 9) >(vw vc)) {
            psh!(q w)
         }
         if and(<(va 9) >(va vc)) {
            psh!(q a)
         }
         if and(<(vs 9) >(vs vc)) {
            psh!(q s)
         }
         if and(<(vd 9) >(vd vc)) {
            psh!(q d)
         }
      }
      return-from(basin visited)
   )
)

(defun basin-count (low) (length (basin low)))

(defun largest-three (seq) (subseq (sort seq '>) 0 3))

(defconstant the-basin (mapcar 'basin-count *lows*))

(princ (apply '* (largest-three the-basin)))
(exit)
