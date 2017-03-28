(>=
 (domatch
  (var patcs)
  (Cons (var x) (var xs))
  (var n))
 (+
  (domatch[Ite]
   (prefix
    (var patcs)
    (Cons (var x) (var xs)))
   (var patcs)
   (Cons (var x) (var xs))
   (var n))
  1))
(>=
 (domatch
  (Cons (var x) (var xs))
  (Nil)
  (var n))
 (+ (Nil) 1))
(>=
 (domatch (Nil) (Nil) (var n))
 (+ (Cons (var n) (Nil)) 1))
(>=
 (eqNatList
  (Cons (var x) (var xs))
  (Cons (var y) (var ys)))
 (+
  (eqNatList[Ite]
   (!EQ (var x) (var y))
   (var y)
   (var ys)
   (var x)
   (var xs))
  1))
(>=
 (eqNatList
  (Cons (var x) (var xs))
  (Nil))
 (+ (False) 1))
(>=
 (eqNatList
  (Nil)
  (Cons (var y) (var ys)))
 (+ (False) 1))
(>=
 (eqNatList (Nil) (Nil))
 (+ (True) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (prefix
  (Cons (var x) (var xs))
  (Nil))
 (+ (False) 1))
(>=
 (prefix
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs)))
 (+
  (and
   (!EQ (var x') (var x))
   (prefix (var xs') (var xs)))
  1))
(>=
 (prefix (Nil) (var cs))
 (+ (True) 1))
(>=
 (strmatch
  (var patstr)
  (var str))
 (+
  (domatch
   (var patstr)
   (var str)
   (Nil))
  1))
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
 (domatch[Ite]
  (False)
  (var patcs)
  (Cons (var x) (var xs))
  (var n))
 (domatch
  (var patcs)
  (var xs)
  (Cons
   (var n)
   (Cons (Nil) (Nil)))))
(>=
 (domatch[Ite]
  (True)
  (var patcs)
  (Cons (var x) (var xs))
  (var n))
 (Cons
  (var n)
  (domatch
   (var patcs)
   (var xs)
   (Cons
    (var n)
    (Cons (Nil) (Nil))))))
(>=
 (eqNatList[Ite]
  (False)
  (var y)
  (var ys)
  (var x)
  (var xs))
 (False))
(>=
 (eqNatList[Ite]
  (True)
  (var y)
  (var ys)
  (var x)
  (var xs))
 (eqNatList (var xs) (var ys)))
(>= (!EQ (var _x1) (var _x2)) 0)
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 (var _x2))
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>=
 (and (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>=
 (domatch
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (domatch[Ite]
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 (var _x1))
(>=
 (eqNatList (var _x1) (var _x2))
 0)
(>=
 (eqNatList[Ite]
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5))
 (var _x1))
(>= (notEmpty (var _x1)) 0)
(>=
 (prefix (var _x1) (var _x2))
 0)
(>=
 (strmatch (var _x1) (var _x2))
 0)
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
