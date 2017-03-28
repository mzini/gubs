(>=
 (even (Cons (var x) (Nil)))
 (+ (False) 1))
(>=
 (even
  (Cons
   (var x')
   (Cons (var x) (var xs))))
 (+ (even (var xs)) 1))
(>= (even (Nil)) (+ (True) 1))
(>=
 (goal (var x) (var y))
 (+
  (and
   (lte (var x) (var y))
   (even (var x)))
  1))
(>=
 (lte
  (Cons (var x) (var xs))
  (Nil))
 (+ (False) 1))
(>=
 (lte
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs)))
 (+ (lte (var xs') (var xs)) 1))
(>=
 (lte (Nil) (var y))
 (+ (True) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (and (False) (False))
 (False))
(>=
 (and (False) (True))
 (False))
(>=
 (and (True) (False))
 (False))
(>= (and (True) (True)) (True))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>=
 (and (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (even (var _x1)) 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>= (lte (var _x1) (var _x2)) 0)
(>= (notEmpty (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
