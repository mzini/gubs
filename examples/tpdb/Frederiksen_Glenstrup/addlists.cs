(>=
 (addlist
  (Cons (var x) (var xs'))
  (Cons (S (0)) (var xs)))
 (+
  (Cons
   (S (var x))
   (addlist (var xs') (var xs)))
  1))
(>=
 (addlist
  (Cons (S (0)) (var xs'))
  (Cons (var x) (var xs)))
 (+
  (Cons
   (S (var x))
   (addlist (var xs') (var xs)))
  1))
(>=
 (addlist (Nil) (var ys))
 (+ (Nil) 1))
(>=
 (goal (var xs) (var ys))
 (+
  (addlist (var xs) (var ys))
  1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>=
 (addlist (var _x1) (var _x2))
 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>= (notEmpty (var _x1)) 0)
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
