(>=
 (comp_f_g#1
  (comp_f_g (var x4) (var x5))
  (comp_f_g (var x2) (var x3))
  (var x1))
 (+
  (comp_f_g#1
   (var x4)
   (var x5)
   (comp_f_g#1
    (var x2)
    (var x3)
    (var x1)))
  1))
(>=
 (comp_f_g#1
  (comp_f_g (var x7) (var x9))
  (cons_x (var x2))
  (var x4))
 (+
  (comp_f_g#1
   (var x7)
   (var x9)
   (Cons (var x2) (var x4)))
  1))
(>=
 (comp_f_g#1
  (cons_x (var x2))
  (comp_f_g (var x5) (var x7))
  (var x3))
 (+
  (Cons
   (var x2)
   (comp_f_g#1
    (var x5)
    (var x7)
    (var x3)))
  1))
(>=
 (comp_f_g#1
  (cons_x (var x5))
  (cons_x (var x2))
  (var x4))
 (+
  (Cons
   (var x5)
   (Cons (var x2) (var x4)))
  1))
(>=
 (main (Leaf (var x4)))
 (+ (Cons (var x4) (Nil)) 1))
(>=
 (main (Node (var x9) (var x5)))
 (+
  (comp_f_g#1
   (walk#1 (var x9))
   (walk#1 (var x5))
   (Nil))
  1))
(>=
 (walk#1 (Leaf (var x2)))
 (+ (cons_x (var x2)) 1))
(>=
 (walk#1
  (Node (var x5) (var x3)))
 (+
  (comp_f_g
   (walk#1 (var x5))
   (walk#1 (var x3)))
  1))
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (Leaf (var _x1)) 0)
(>= (Nil) 0)
(>=
 (Node (var _x1) (var _x2))
 0)
(>=
 (comp_f_g (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>=
 (comp_f_g#1
  (var _x1)
  (var _x2)
  (var _x3))
 (+
  (var _x1)
  (+ (var _x2) (var _x3))))
(>= (cons_x (var _x1)) 0)
(>= (main (var _x1)) 0)
(>= (walk#1 (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>=
 (+ (var _x1) (_f2))
 (Leaf (var _x1)))
(>= (_f3) (Nil))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f4))
 (Node (var _x1) (var _x2)))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f5))
 (comp_f_g (var _x1) (var _x2)))
(>=
 (+ (var _x1) (_f6))
 (cons_x (var _x1)))
