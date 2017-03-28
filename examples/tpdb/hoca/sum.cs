(>=
 (fold#3
  (Cons (var x4) (var x2)))
 (+
  (plus#2
   (var x4)
   (fold#3 (var x2)))
  1))
(>= (fold#3 (Nil)) (+ (0) 1))
(>=
 (main (var x1))
 (+ (fold#3 (var x1)) 1))
(>=
 (plus#2 (0) (var x12))
 (+ (var x12) 1))
(>=
 (plus#2 (S (var x4)) (var x2))
 (+
  (S (plus#2 (var x4) (var x2)))
  1))
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>= (S (var _x1)) (var _x1))
(>= (fold#3 (var _x1)) 0)
(>= (main (var _x1)) 0)
(>=
 (plus#2 (var _x1) (var _x2))
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
