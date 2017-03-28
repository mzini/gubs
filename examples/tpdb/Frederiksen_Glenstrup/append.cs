(>=
 (append
  (Cons (var x) (var xs))
  (var ys))
 (+
  (Cons
   (var x)
   (append (var xs) (var ys)))
  1))
(>=
 (append (Nil) (var ys))
 (+ (var ys) 1))
(>=
 (goal (var x) (var y))
 (+ (append (var x) (var y)) 1))
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (Nil) 0)
(>=
 (append (var _x1) (var _x2))
 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
