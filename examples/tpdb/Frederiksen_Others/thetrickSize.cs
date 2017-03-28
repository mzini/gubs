(>=
 (f
  (var x)
  (Cons (var x') (var xs)))
 (+
  (f[Ite][False][Ite]
   (lt0
    (var x)
    (Cons (Nil) (Nil)))
   (var x)
   (Cons (var x') (var xs)))
  1))
(>=
 (f (var x) (Nil))
 (+
  (Cons
   (Nil)
   (Cons
    (Nil)
    (Cons
     (Nil)
     (Cons (Nil) (Nil)))))
  1))
(>=
 (g
  (var x)
  (Cons (var x') (var xs)))
 (+
  (g[Ite][False][Ite]
   (lt0
    (var x)
    (Cons (Nil) (Nil)))
   (var x)
   (Cons (var x') (var xs)))
  1))
(>=
 (g (var x) (Nil))
 (+
  (Cons
   (Nil)
   (Cons
    (Nil)
    (Cons
     (Nil)
     (Cons (Nil) (Nil)))))
  1))
(>=
 (goal (var x) (var y))
 (+
  (Cons
   (f (var x) (var y))
   (Cons
    (g (var x) (var y))
    (Nil)))
  1))
(>=
 (lt0 (var x) (Nil))
 (+ (False) 1))
(>=
 (lt0
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs)))
 (+ (lt0 (var xs') (var xs)) 1))
(>=
 (lt0
  (Nil)
  (Cons (var x') (var xs)))
 (+ (True) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (number4 (var n))
 (+
  (Cons
   (Nil)
   (Cons
    (Nil)
    (Cons
     (Nil)
     (Cons (Nil) (Nil)))))
  1))
(>=
 (f[Ite][False][Ite]
  (False)
  (Cons (var x) (var xs))
  (var y))
 (f
  (var xs)
  (Cons
   (Cons (Nil) (Nil))
   (var y))))
(>=
 (f[Ite][False][Ite]
  (True)
  (var x')
  (Cons (var x) (var xs)))
 (f (var x') (var xs)))
(>=
 (g[Ite][False][Ite]
  (False)
  (Cons (var x) (var xs))
  (var y))
 (g
  (var xs)
  (Cons
   (Cons (Nil) (Nil))
   (var y))))
(>=
 (g[Ite][False][Ite]
  (True)
  (var x')
  (Cons (var x) (var xs)))
 (g (var x') (var xs)))
(>=
 (Cons (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>= (f (var _x1) (var _x2)) 0)
(>=
 (f[Ite][False][Ite]
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x1))
(>= (g (var _x1) (var _x2)) 0)
(>=
 (g[Ite][False][Ite]
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x1))
(>=
 (goal (var _x1) (var _x2))
 0)
(>= (lt0 (var _x1) (var _x2)) 0)
(>= (notEmpty (var _x1)) 0)
(>= (number4 (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
