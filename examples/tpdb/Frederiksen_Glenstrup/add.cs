(>=
 (add0 (var x) (Nil))
 (+ (var x) 1))
(>=
 (add0
  (var x')
  (Cons (var x) (var xs)))
 (+
  (add0
   (Cons
    (Cons (Nil) (Nil))
    (var x'))
   (var xs))
  1))
(>=
 (goal (var x) (var y))
 (+ (add0 (var x) (var y)) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>=
 (add0 (var _x1) (var _x2))
 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>= (notEmpty (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
