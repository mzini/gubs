(>=
 (main (var x5) (var x12))
 (+
  (map#2
   (plus_x (var x12))
   (var x5))
  1))
(>=
 (map#2 (plus_x (var x2)) (Nil))
 (+ (Nil) 1))
(>=
 (map#2
  (plus_x (var x6))
  (Cons (var x4) (var x2)))
 (+
  (Cons
   (plus_x#1 (var x6) (var x4))
   (map#2
    (plus_x (var x6))
    (var x2)))
  1))
(>=
 (plus_x#1 (0) (var x8))
 (+ (var x8) 1))
(>=
 (plus_x#1
  (S (var x12))
  (var x14))
 (+
  (S
   (plus_x#1 (var x12) (var x14)))
  1))
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (Nil) 0)
(>= (S (var _x1)) (var _x1))
(>=
 (main (var _x1) (var _x2))
 0)
(>=
 (map#2 (var _x1) (var _x2))
 0)
(>= (plus_x (var _x1)) 0)
(>=
 (plus_x#1 (var _x1) (var _x2))
 0)
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
 (+ (var _x1) (_f5))
 (plus_x (var _x1)))
