(>=
 (foldr#3
  (Cons (var x16) (var x6)))
 (+
  (step_x_f
   (rev_l)
   (var x16)
   (foldr#3 (var x6)))
  1))
(>=
 (foldr#3 (Nil))
 (+ (fleft_op_e_xs_1) 1))
(>=
 (main (Cons (var x8) (var x9)))
 (+
  (step_x_f#1
   (rev_l)
   (var x8)
   (foldr#3 (var x9))
   (Nil))
  1))
(>= (main (Nil)) (+ (Nil) 1))
(>=
 (rev_l#2 (var x8) (var x10))
 (+
  (Cons (var x10) (var x8))
  1))
(>=
 (step_x_f#1
  (rev_l)
  (var x5)
  (fleft_op_e_xs_1)
  (var x3))
 (+
  (rev_l#2 (var x3) (var x5))
  1))
(>=
 (step_x_f#1
  (rev_l)
  (var x5)
  (step_x_f
   (var x2)
   (var x3)
   (var x4))
  (var x1))
 (+
  (step_x_f#1
   (var x2)
   (var x3)
   (var x4)
   (rev_l#2 (var x1) (var x5)))
  1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>= (fleft_op_e_xs_1) 0)
(>= (foldr#3 (var _x1)) 0)
(>= (main (var _x1)) 0)
(>= (rev_l) 0)
(>=
 (rev_l#2 (var _x1) (var _x2))
 0)
(>=
 (step_x_f
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x3))
(>=
 (step_x_f#1
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 (+ (var _x3) (var _x4)))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
(>= (_f3) (fleft_op_e_xs_1))
(>= (_f4) (rev_l))
(>=
 (+
  (+
   (var _x1)
   (+ (var _x2) (var _x3)))
  (_f5))
 (step_x_f
  (var _x1)
  (var _x2)
  (var _x3)))
