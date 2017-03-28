(>=
 (goal (var xs) (var ys))
 (+
  (revapp (var xs) (var ys))
  1))
(>=
 (revapp
  (Cons (var x) (var xs))
  (var rest))
 (+
  (revapp
   (var xs)
   (Cons (var x) (var rest)))
  1))
(>=
 (revapp (Nil) (var rest))
 (+ (var rest) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>=
 (revapp (var _x1) (var _x2))
 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
