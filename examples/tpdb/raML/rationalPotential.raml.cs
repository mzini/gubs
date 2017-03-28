(>=
 (group3 (var @l))
 (+ (group3#1 (var @l)) 1))
(>=
 (group3#1
  (:: (var @x) (var @xs)))
 (+
  (group3#2 (var @xs) (var @x))
  1))
(>=
 (group3#1 (nil))
 (+ (nil) 1))
(>=
 (group3#2
  (:: (var @y) (var @ys))
  (var @x))
 (+
  (group3#3
   (var @ys)
   (var @x)
   (var @y))
  1))
(>=
 (group3#2 (nil) (var @x))
 (+ (nil) 1))
(>=
 (group3#3
  (:: (var @z) (var @zs))
  (var @x)
  (var @y))
 (+
  (::
   (tuple#3
    (var @x)
    (var @y)
    (var @z))
   (group3 (var @zs)))
  1))
(>=
 (group3#3
  (nil)
  (var @x)
  (var @y))
 (+ (nil) 1))
(>=
 (zip3
  (var @l1)
  (var @l2)
  (var @l3))
 (+
  (zip3#1
   (var @l1)
   (var @l2)
   (var @l3))
  1))
(>=
 (zip3#1
  (:: (var @x) (var @xs))
  (var @l2)
  (var @l3))
 (+
  (zip3#2
   (var @l2)
   (var @l3)
   (var @x)
   (var @xs))
  1))
(>=
 (zip3#1
  (nil)
  (var @l2)
  (var @l3))
 (+ (nil) 1))
(>=
 (zip3#2
  (:: (var @y) (var @ys))
  (var @l3)
  (var @x)
  (var @xs))
 (+
  (zip3#3
   (var @l3)
   (var @x)
   (var @xs)
   (var @y)
   (var @ys))
  1))
(>=
 (zip3#2
  (nil)
  (var @l3)
  (var @x)
  (var @xs))
 (+ (nil) 1))
(>=
 (zip3#3
  (:: (var @z) (var @zs))
  (var @x)
  (var @xs)
  (var @y)
  (var @ys))
 (+
  (::
   (tuple#3
    (var @x)
    (var @y)
    (var @z))
   (zip3
    (var @xs)
    (var @ys)
    (var @zs)))
  1))
(>=
 (zip3#3
  (nil)
  (var @x)
  (var @xs)
  (var @y)
  (var @ys))
 (+ (nil) 1))
(>=
 (:: (var _x1) (var _x2))
 (var _x2))
(>= (group3 (var _x1)) 0)
(>= (group3#1 (var _x1)) 0)
(>=
 (group3#2 (var _x1) (var _x2))
 0)
(>=
 (group3#3
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>= (nil) 0)
(>=
 (tuple#3
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (zip3
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (zip3#1
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (zip3#2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (zip3#3
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5))
 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (:: (var _x1) (var _x2)))
(>= (_f2) (nil))
(>=
 (+
  (+
   (var _x1)
   (+ (var _x2) (var _x3)))
  (_f3))
 (tuple#3
  (var _x1)
  (var _x2)
  (var _x3)))
