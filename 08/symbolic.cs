// C# because the syntax highlighting

//  AAAA
// B    C
// B    C
//  DDDD
// E    F
// E    F
//  GGGG

// We know that:
A1 := {C F}
A2 := {A C D E G}
A3 := {A C D F G}
A4 := {B C D F}
A5 := {A B D F G}
A6 := {A B D E F G}
A7 := {A C F}
A8 := {A B C D E F G}
A9 := {A B C D F G}
A0 := {A B C E F G}

A := {A1 A2 A3 A4 A5 A6 A7 A8 A9 A0}

// Given the set:
Q1 := {c f}
Q2 := {a c d e g}
Q3 := {a c d f g}
Q4 := {b c d f}
Q5 := {a b d f g}
Q6 := {a b d e f g}
Q7 := {a c f}
Q8 := {a b c d e f g}
Q9 := {a b d e f g}
Q0 := {a b c e f g}
Q := {Q1 Q2 Q3 Q4 Q5 Q6 Q7 Q8 Q9 Q0}

// Show that
// A = a, B = b, C = c, D = d, E = e, F = f, G = g.
// Alternatively, show that
// A1 = Q1, A2 = Q2, A3 = Q3, A4 = Q4, A5 = Q5,
// A6 = Q6, A7 = Q7, A8 = Q8, A9 = Q9, A0 = Q0,

filter (size 2) A = filter (size 2) Q
A1 = Q1

filter (size 3) A = filter (size 3) Q
A7 = Q7

filter (size 4) A = filter (size 4) Q
A4 = Q4

filter (size 7) A = filter (size 7) Q
A8 = Q8

filter (size 5) A = filter (size 5) Q
{A2 A3 A5} = {Q2 Q3 Q5}

filter (subset A1) {A2 A3 A5} = filter (subset Q1) {Q2 Q3 Q5}
{A2 A5} = {Q2 Q5}, A3 = Q3

intersect @explode{A2 A3 A5} = intersect @explode{Q2 Q3 Q5}
{A D G} = {a d g}

intersect {A D G} A4 = intersect {a d g} Q4
D = d

filter (size 6) A = filter (size 6) Q
{A6 A9 A0} = {Q6 Q9 Q0}

filter (subset A1) {A6 A9 A0} = filter (subset Q1) {Q6 Q9 Q0}
A6 = Q6, {A9 A0} = {Q9 Q0}

filter (haselement D) {A9 A0} = filter (haselement d) {Q9 Q0}
A0 = Q0, A9 = Q9

A8 - A9 = Q8 - Q9
E = e

// we know that
{A2 A5} = {Q2 A5}

filter (haselement E) {A2 A5} = filter (haselement e) {Q2 A5}
A5 = Q5, A2 = Q2

A1 = Q1
A2 = Q2
A3 = Q3
A4 = Q4
A5 = Q5
A6 = Q6
A7 = Q7
A8 = Q8
A9 = Q9
A0 = Q0

// qed
