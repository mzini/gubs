(>=
 (goal (var x) (var xs))
 (+
  (member (var x) (var xs))
  1))
(>=
 (member (var x) (Nil))
 (+ (False) 1))
(>=
 (member
  (var x')
  (Cons (var x) (var xs)))
 (+
  (member[Ite][True][Ite]
   (!EQ (var x') (var x))
   (var x')
   (Cons (var x) (var xs)))
  1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>= (!EQ (0) (0)) (True))
(>=
 (!EQ (0) (S (var y)))
 (False))
(>=
 (!EQ (S (var x)) (0))
 (False))
(>=
 (!EQ (S (var x)) (S (var y)))
 (!EQ (var x) (var y)))
(>=
 (member[Ite][True][Ite]
  (False)
  (var x')
  (Cons (var x) (var xs)))
 (member (var x') (var xs)))
(>=
 (member[Ite][True][Ite]
  (True)
  (var x)
  (var xs))
 (True))
(>= (!EQ (var _x1) (var _x2)) 0)
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>=
 (goal (var _x1) (var _x2))
 0)
(>=
 (member (var _x1) (var _x2))
 0)
(>=
 (member[Ite][True][Ite]
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x1))
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
