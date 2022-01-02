(load "../shared/clike.cl")

(defconstant pairs #(
   (#\( #\))
   (#\[ #\])
   (#\{ #\})
   (#\< #\>)
))

(defvar *scoring* (make-hash-table))
(setf (gethash (cadr (aref pairs 0)) *scoring*) 1)
(setf (gethash (cadr (aref pairs 1)) *scoring*) 2)
(setf (gethash (cadr (aref pairs 2)) *scoring*) 3)
(setf (gethash (cadr (aref pairs 3)) *scoring*) 4)

(clike
   let scores = nil

   let fd = open("input.txt")
   let line = nil
   while(next-line) line = read-line(fd nil) {
      let stack = nil

      let i = 0
      let l = length(line)
      while <(i l) {
         let c = aref(line i)
         let p = 0

         while(pair-search) <(p length(pairs)) {
            let pair  = aref(pairs p)
            let open  = car(pair)
            let close = cadr(pair)

            if eq(c open) {
               stack = cons(close stack)
               break(pair-search)
            }

            if eq(c close) {
               if eq(car(stack) close) {
                  stack = cdr(stack)
                  break(pair-search)
               } else {
                  format(t "expected stack to have `~C', not `~C'~%" open car(stack))
                  continue(next-line)
               }
            }

            p = +(p 1)
         }
         i = +(i 1)
      }

      let score = 0
      while stack {
         score = +(*(5 score) gethash(car!(stack) *scoring*))
      }
      format(t "line had a score of ~D~%" score)
      psh!(scores score)
   }

   ; find the middle score
   scores = sort(scores quote(<))
   let middle-idx = floor(/(length(scores) 2))
   princ(elt(scores middle-idx))
   terpri <- nil
   exit <- nil
)

