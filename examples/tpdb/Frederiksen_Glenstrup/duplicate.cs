(>=
 (duplicate
  (Cons (var x) (var xs)))
 (+
  (Cons
   (var x)
   (Cons
    (var x)
    (duplicate (var xs))))
  1))
(>=
 (duplicate (Nil))
 (+ (Nil) 1))
(>=
 (goal (var x))
 (+ (duplicate (var x)) 1))
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (Nil) 0)
(>= (duplicate (var _x1)) 0)
(>= (goal (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
