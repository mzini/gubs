(>=
 (app
  (Cons (var x) (var xs))
  (var ys))
 (+
  (Cons
   (var x)
   (app (var xs) (var ys)))
  1))
(>=
 (app (Nil) (var ys))
 (+ (var ys) 1))
(>=
 (goal (var xs))
 (+ (naiverev (var xs)) 1))
(>=
 (naiverev
  (Cons (var x) (var xs)))
 (+
  (app
   (naiverev (var xs))
   (Cons (var x) (Nil)))
  1))
(>=
 (naiverev (Nil))
 (+ (Nil) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>=
 (app (var _x1) (var _x2))
 (var _x1))
(>= (goal (var _x1)) 0)
(>= (naiverev (var _x1)) 0)
(>= (notEmpty (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
