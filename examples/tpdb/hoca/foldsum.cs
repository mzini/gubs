(>=
 (comp_f_g#1
  (plus_x (var x3))
  (comp_f_g (var x1) (var x2))
  (0))
 (+
  (plus_x#1
   (var x3)
   (comp_f_g#1
    (var x1)
    (var x2)
    (0)))
  1))
(>=
 (comp_f_g#1
  (plus_x (var x3))
  (id)
  (0))
 (+ (plus_x#1 (var x3) (0)) 1))
(>=
 (foldr#3
  (Cons (var x32) (var x6)))
 (+
  (comp_f_g
   (var x32)
   (foldr#3 (var x6)))
  1))
(>= (foldr#3 (Nil)) (+ (id) 1))
(>=
 (foldr_f#3
  (Cons (var x16) (var x5))
  (var x24))
 (+
  (comp_f_g#1
   (var x16)
   (foldr#3 (var x5))
   (var x24))
  1))
(>=
 (foldr_f#3 (Nil) (0))
 (+ (0) 1))
(>=
 (main (var x3))
 (+
  (foldr_f#3
   (map#2 (var x3))
   (0))
  1))
(>=
 (map#2
  (Cons (var x16) (var x6)))
 (+
  (Cons
   (plus_x (var x16))
   (map#2 (var x6)))
  1))
(>= (map#2 (Nil)) (+ (Nil) 1))
(>=
 (plus_x#1 (0) (var x6))
 (+ (var x6) 1))
(>=
 (plus_x#1
  (S (var x8))
  (var x10))
 (+
  (S
   (plus_x#1 (var x8) (var x10)))
  1))
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (Nil) 0)
(>= (S (var _x1)) (var _x1))
(>=
 (comp_f_g (var _x1) (var _x2))
 (var _x2))
(>=
 (comp_f_g#1
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x2))
(>= (foldr#3 (var _x1)) 0)
(>=
 (foldr_f#3 (var _x1) (var _x2))
 (var _x1))
(>= (id) 0)
(>= (main (var _x1)) 0)
(>= (map#2 (var _x1)) 0)
(>= (plus_x (var _x1)) 0)
(>=
 (plus_x#1 (var _x1) (var _x2))
 (var _x2))
(>= (_f1) (0))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f2))
 (Cons (var _x1) (var _x2)))
(>= (_f3) (Nil))
(>=
 (+ (var _x1) (_f4))
 (S (var _x1)))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f5))
 (comp_f_g (var _x1) (var _x2)))
(>= (_f6) (id))
(>=
 (+ (var _x1) (_f7))
 (plus_x (var _x1)))
