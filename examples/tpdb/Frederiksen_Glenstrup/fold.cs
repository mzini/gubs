(>=
 (fold (var a) (var xs))
 (+
  (Cons
   (foldl (var a) (var xs))
   (Cons
    (foldr (var a) (var xs))
    (Nil)))
  1))
(>=
 (foldl (var a) (Nil))
 (+ (var a) 1))
(>=
 (foldl
  (var x)
  (Cons (S (0)) (var xs)))
 (+
  (foldl (S (var x)) (var xs))
  1))
(>=
 (foldl
  (S (0))
  (Cons (var x) (var xs)))
 (+
  (foldl (S (var x)) (var xs))
  1))
(>=
 (foldr
  (var a)
  (Cons (var x) (var xs)))
 (+
  (op
   (var x)
   (foldr (var a) (var xs)))
  1))
(>=
 (foldr (var a) (Nil))
 (+ (var a) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (op (var x) (S (0)))
 (+ (S (var x)) 1))
(>=
 (op (S (0)) (var y))
 (+ (S (var y)) 1))
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>=
 (fold (var _x1) (var _x2))
 0)
(>=
 (foldl (var _x1) (var _x2))
 0)
(>=
 (foldr (var _x1) (var _x2))
 0)
(>= (notEmpty (var _x1)) 0)
(>=
 (op (var _x1) (var _x2))
 (var _x2))
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
