(>=
 (anchored
  (Cons (var x) (var xs))
  (var y))
 (+
  (anchored
   (var xs)
   (Cons
    (Cons (Nil) (Nil))
    (var y)))
  1))
(>=
 (anchored (Nil) (var y))
 (+ (var y) 1))
(>=
 (goal (var x) (var y))
 (+
  (anchored (var x) (var y))
  1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>=
 (anchored (var _x1) (var _x2))
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
