(load "../shared/clike.cl")

(defconstant pairs #(
   #(#\( #\) 3)
   #(#\[ #\] 57)
   #(#\{ #\} 1197)
   #(#\< #\> 25137)
))

(clike
   let score = 0
   let fd = open("input.txt")
   let line = nil
   while line = read-line(fd nil) {
      princ(line)
      terpri <- nil

      let stack = nil

      let i = 0
      let l = length(line)
      while(this-line) <(i l) {
         let c = aref(line i)
         let p = 0

         while(pair-search) <(p length(pairs)) {
            let pair   = aref(pairs p)
            let open   = aref(pair 0)
            let close  = aref(pair 1)
            let points = aref(pair 2)

            if eq(c open) {
               stack = cons(open stack)

               break(pair-search)
            } else
            if eq(c close) {
               if eq(car(stack) open) {
                  stack = cdr(stack)

                  break(pair-search)
               } else {
                  format(t "expected stack to have `~C', not `~C'~%" open car(stack))
                  score = +(score points)
                  break(this-line)
               }
            }

            p = +(p 1)
         }
         i = +(i 1)
      }
   }
   princ(score)
   terpri <- nil
   exit <- nil
)

