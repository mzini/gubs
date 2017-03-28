(>=
 (goal (var xs))
 (+ (ordered (var xs)) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (ordered (Cons (var x) (Nil)))
 (+ (True) 1))
(>=
 (ordered
  (Cons
   (var x')
   (Cons (var x) (var xs))))
 (+
  (ordered[Ite]
   (< (var x') (var x))
   (Cons
    (var x')
    (Cons (var x) (var xs))))
  1))
(>=
 (ordered (Nil))
 (+ (True) 1))
(>= (< (var x) (0)) (False))
(>= (< (0) (S (var y))) (True))
(>=
 (< (S (var x)) (S (var y)))
 (< (var x) (var y)))
(>=
 (ordered[Ite] (False) (var xs))
 (False))
(>=
 (ordered[Ite]
  (True)
  (Cons
   (var x')
   (Cons (var x) (var xs))))
 (ordered (var xs)))
(>= (0) 0)
(>= (< (var _x1) (var _x2)) 0)
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>= (goal (var _x1)) 0)
(>= (notEmpty (var _x1)) 0)
(>= (ordered (var _x1)) 0)
(>=
 (ordered[Ite]
  (var _x1)
  (var _x2))
 (var _x1))
(>= (_f1) (0))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f2))
 (Cons (var _x1) (var _x2)))
(>= (_f3) (False))
(>= (_f4) (Nil))
(>=
 (+ (var _x1) (_f5))
 (S (var _x1)))
(>= (_f6) (True))
