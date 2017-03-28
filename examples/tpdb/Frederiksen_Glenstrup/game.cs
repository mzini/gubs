(>=
 (@
  (Cons (var x) (var xs))
  (var ys))
 (+
  (Cons
   (var x)
   (@ (var xs) (var ys)))
  1))
(>=
 (@ (Nil) (var ys))
 (+ (var ys) 1))
(>=
 (equal (Capture) (Capture))
 (+ (True) 1))
(>=
 (equal (Capture) (Swap))
 (+ (False) 1))
(>=
 (equal (Swap) (Capture))
 (+ (False) 1))
(>=
 (equal (Swap) (Swap))
 (+ (True) 1))
(>=
 (game
  (var p1)
  (var p2)
  (Cons (Swap) (var xs)))
 (+
  (game
   (var p2)
   (var p1)
   (var xs))
  1))
(>=
 (game (var p1) (var p2) (Nil))
 (+ (@ (var p1) (var p2)) 1))
(>=
 (game
  (var p1)
  (Cons (var x') (var xs'))
  (Cons (Capture) (var xs)))
 (+
  (game
   (Cons (var x') (var p1))
   (var xs')
   (var xs))
  1))
(>=
 (goal
  (var p1)
  (var p2)
  (var moves))
 (+
  (game
   (var p1)
   (var p2)
   (var moves))
  1))
(>= (@ (var _x1) (var _x2)) 0)
(>= (Capture) 0)
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (False) 0)
(>= (Nil) 0)
(>= (Swap) 0)
(>= (True) 0)
(>=
 (equal (var _x1) (var _x2))
 0)
(>=
 (game
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (goal
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>= (_f1) (Capture))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f2))
 (Cons (var _x1) (var _x2)))
(>= (_f3) (False))
(>= (_f4) (Nil))
(>= (_f5) (Swap))
(>= (_f6) (True))
