(>=
 (loop
  (Cons (var x) (var xs))
  (Nil)
  (var pp)
  (var ss))
 (+ (False) 1))
(>=
 (loop
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs))
  (var pp)
  (var ss))
 (+
  (loop[Ite]
   (!EQ (var x') (var x))
   (Cons (var x') (var xs'))
   (Cons (var x) (var xs))
   (var pp)
   (var ss))
  1))
(>=
 (loop
  (Nil)
  (var s)
  (var pp)
  (var ss))
 (+ (True) 1))
(>=
 (match1 (var p) (var s))
 (+
  (loop
   (var p)
   (var s)
   (var p)
   (var s))
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
 (loop[Ite]
  (False)
  (var p)
  (var s)
  (var pp)
  (Cons (var x) (var xs)))
 (loop
  (var pp)
  (var xs)
  (var pp)
  (var xs)))
(>=
 (loop[Ite]
  (True)
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs))
  (var pp)
  (var ss))
 (loop
  (var xs')
  (var xs)
  (var pp)
  (var ss)))
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
 (loop
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (loop[Ite]
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5))
 (var _x1))
(>=
 (match1 (var _x1) (var _x2))
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
