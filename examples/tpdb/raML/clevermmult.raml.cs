(>=
 (* (var @x) (var @y))
 (+
  (#mult (var @x) (var @y))
  1))
(>=
 (+ (var @x) (var @y))
 (+ (#add (var @x) (var @y)) 1))
(>=
 (computeLine
  (var @line)
  (var @m)
  (var @acc))
 (+
  (computeLine#1
   (var @line)
   (var @acc)
   (var @m))
  1))
(>=
 (computeLine#1
  (:: (var @x) (var @xs))
  (var @acc)
  (var @m))
 (+
  (computeLine#2
   (var @m)
   (var @acc)
   (var @x)
   (var @xs))
  1))
(>=
 (computeLine#1
  (nil)
  (var @acc)
  (var @m))
 (+ (var @acc) 1))
(>=
 (computeLine#2
  (:: (var @l) (var @ls))
  (var @acc)
  (var @x)
  (var @xs))
 (+
  (computeLine
   (var @xs)
   (var @ls)
   (lineMult
    (var @x)
    (var @l)
    (var @acc)))
  1))
(>=
 (computeLine#2
  (nil)
  (var @acc)
  (var @x)
  (var @xs))
 (+ (nil) 1))
(>=
 (lineMult
  (var @n)
  (var @l1)
  (var @l2))
 (+
  (lineMult#1
   (var @l1)
   (var @l2)
   (var @n))
  1))
(>=
 (lineMult#1
  (:: (var @x) (var @xs))
  (var @l2)
  (var @n))
 (+
  (lineMult#2
   (var @l2)
   (var @n)
   (var @x)
   (var @xs))
  1))
(>=
 (lineMult#1
  (nil)
  (var @l2)
  (var @n))
 (+ (nil) 1))
(>=
 (lineMult#2
  (:: (var @y) (var @ys))
  (var @n)
  (var @x)
  (var @xs))
 (+
  (::
   (+
    (* (var @x) (var @n))
    (var @y))
   (lineMult
    (var @n)
    (var @xs)
    (var @ys)))
  1))
(>=
 (lineMult#2
  (nil)
  (var @n)
  (var @x)
  (var @xs))
 (+
  (::
   (* (var @x) (var @n))
   (lineMult
    (var @n)
    (var @xs)
    (nil)))
  1))
(>=
 (matrixMult
  (var @m1)
  (var @m2))
 (+
  (matrixMult#1
   (var @m1)
   (var @m2))
  1))
(>=
 (matrixMult#1
  (:: (var @l) (var @ls))
  (var @m2))
 (+
  (::
   (computeLine
    (var @l)
    (var @m2)
    (nil))
   (matrixMult
    (var @ls)
    (var @m2)))
  1))
(>=
 (matrixMult#1 (nil) (var @m2))
 (+ (nil) 1))
(>=
 (#add (#0) (var @y))
 (var @y))
(>=
 (#add
  (#neg (#s (#0)))
  (var @y))
 (#pred (var @y)))
(>=
 (#add
  (#neg (#s (#s (var @x))))
  (var @y))
 (#pred
  (#add
   (#pos (#s (var @x)))
   (var @y))))
(>=
 (#add
  (#pos (#s (#0)))
  (var @y))
 (#succ (var @y)))
(>=
 (#add
  (#pos (#s (#s (var @x))))
  (var @y))
 (#succ
  (#add
   (#pos (#s (var @x)))
   (var @y))))
(>= (#mult (#0) (#0)) (#0))
(>=
 (#mult (#0) (#neg (var @y)))
 (#0))
(>=
 (#mult (#0) (#pos (var @y)))
 (#0))
(>=
 (#mult (#neg (var @x)) (#0))
 (#0))
(>=
 (#mult
  (#neg (var @x))
  (#neg (var @y)))
 (#pos
  (#natmult (var @x) (var @y))))
(>=
 (#mult
  (#neg (var @x))
  (#pos (var @y)))
 (#neg
  (#natmult (var @x) (var @y))))
(>=
 (#mult (#pos (var @x)) (#0))
 (#0))
(>=
 (#mult
  (#pos (var @x))
  (#neg (var @y)))
 (#neg
  (#natmult (var @x) (var @y))))
(>=
 (#mult
  (#pos (var @x))
  (#pos (var @y)))
 (#pos
  (#natmult (var @x) (var @y))))
(>=
 (#natmult (#0) (var @y))
 (#0))
(>=
 (#natmult
  (#s (var @x))
  (var @y))
 (#add
  (#pos (var @y))
  (#natmult (var @x) (var @y))))
(>=
 (#pred (#0))
 (#neg (#s (#0))))
(>=
 (#pred (#neg (#s (var @x))))
 (#neg (#s (#s (var @x)))))
(>=
 (#pred (#pos (#s (#0))))
 (#0))
(>=
 (#pred
  (#pos (#s (#s (var @x)))))
 (#pos (#s (var @x))))
(>=
 (#succ (#0))
 (#pos (#s (#0))))
(>=
 (#succ (#neg (#s (#0))))
 (#0))
(>=
 (#succ
  (#neg (#s (#s (var @x)))))
 (#neg (#s (var @x))))
(>=
 (#succ (#pos (#s (var @x))))
 (#pos (#s (#s (var @x)))))
(>= (#0) 0)
(>=
 (#add (var _x1) (var _x2))
 (var _x2))
(>=
 (#mult (var _x1) (var _x2))
 0)
(>=
 (#natmult (var _x1) (var _x2))
 0)
(>= (#neg (var _x1)) (var _x1))
(>= (#pos (var _x1)) (var _x1))
(>= (#pred (var _x1)) (var _x1))
(>= (#s (var _x1)) 0)
(>= (#succ (var _x1)) (var _x1))
(>= (* (var _x1) (var _x2)) 0)
(>=
 (+ (var _x1) (var _x2))
 (var _x1))
(>=
 (:: (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>=
 (computeLine
  (var _x1)
  (var _x2)
  (var _x3))
 (var _x3))
(>=
 (computeLine#1
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (computeLine#2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (lineMult
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (lineMult#1
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (lineMult#2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (matrixMult
  (var _x1)
  (var _x2))
 0)
(>=
 (matrixMult#1
  (var _x1)
  (var _x2))
 0)
(>= (nil) 0)
(>= (_f1) (#0))
(>=
 (+ (var _x1) (_f2))
 (#neg (var _x1)))
(>=
 (+ (var _x1) (_f3))
 (#pos (var _x1)))
(>=
 (+ (var _x1) (_f4))
 (#s (var _x1)))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f5))
 (:: (var _x1) (var _x2)))
(>= (_f6) (nil))
