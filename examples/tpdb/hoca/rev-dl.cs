(>=
 (comp_f_g#1
  (comp_f_g (var x7) (var x9))
  (walk_xs_3 (var x8))
  (var x12))
 (+
  (comp_f_g#1
   (var x7)
   (var x9)
   (Cons (var x8) (var x12)))
  1))
(>=
 (comp_f_g#1
  (walk_xs)
  (walk_xs_3 (var x8))
  (var x12))
 (+
  (Cons (var x8) (var x12))
  1))
(>=
 (main (Cons (var x4) (var x5)))
 (+
  (comp_f_g#1
   (walk#1 (var x5))
   (walk_xs_3 (var x4))
   (Nil))
  1))
(>= (main (Nil)) (+ (Nil) 1))
(>=
 (walk#1
  (Cons (var x4) (var x3)))
 (+
  (comp_f_g
   (walk#1 (var x3))
   (walk_xs_3 (var x4)))
  1))
(>=
 (walk#1 (Nil))
 (+ (walk_xs) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>=
 (comp_f_g (var _x1) (var _x2))
 (var _x1))
(>=
 (comp_f_g#1
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x1))
(>= (main (var _x1)) 0)
(>= (walk#1 (var _x1)) 0)
(>= (walk_xs) 0)
(>= (walk_xs_3 (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f3))
 (comp_f_g (var _x1) (var _x2)))
(>= (_f4) (walk_xs))
(>=
 (+ (var _x1) (_f5))
 (walk_xs_3 (var _x1)))
