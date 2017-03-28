(>=
 (goal (var xs) (var ys))
 (+
  (merge (var xs) (var ys))
  1))
(>=
 (merge
  (Cons (var x) (var xs))
  (Nil))
 (+ (Cons (var x) (var xs)) 1))
(>=
 (merge
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs)))
 (+
  (merge[Ite]
   (<= (var x') (var x))
   (Cons (var x') (var xs'))
   (Cons (var x) (var xs)))
  1))
(>=
 (merge (Nil) (var ys))
 (+ (var ys) 1))
(>= (<= (0) (var y)) (True))
(>=
 (<= (S (var x)) (0))
 (False))
(>=
 (<= (S (var x)) (S (var y)))
 (<= (var x) (var y)))
(>=
 (merge[Ite]
  (False)
  (var xs')
  (Cons (var x) (var xs)))
 (Cons
  (var x)
  (merge (var xs') (var xs))))
(>=
 (merge[Ite]
  (True)
  (Cons (var x) (var xs))
  (var ys))
 (Cons
  (var x)
  (merge (var xs) (var ys))))
(>= (0) 0)
(>= (<= (var _x1) (var _x2)) 0)
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>=
 (merge (var _x1) (var _x2))
 0)
(>=
 (merge[Ite]
  (var _x1)
  (var _x2)
  (var _x3))
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
