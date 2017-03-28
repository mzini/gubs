(>=
 (append (var @l1) (var @l2))
 (+
  (append#1 (var @l1) (var @l2))
  1))
(>=
 (append#1
  (:: (var @x) (var @xs))
  (var @l2))
 (+
  (::
   (var @x)
   (append (var @xs) (var @l2)))
  1))
(>=
 (append#1 (nil) (var @l2))
 (+ (var @l2) 1))
(>=
 (appendAll (var @l))
 (+ (appendAll#1 (var @l)) 1))
(>=
 (appendAll#1
  (:: (var @l1) (var @ls)))
 (+
  (append
   (var @l1)
   (appendAll (var @ls)))
  1))
(>=
 (appendAll#1 (nil))
 (+ (nil) 1))
(>=
 (appendAll2 (var @l))
 (+ (appendAll2#1 (var @l)) 1))
(>=
 (appendAll2#1
  (:: (var @l1) (var @ls)))
 (+
  (append
   (appendAll (var @l1))
   (appendAll2 (var @ls)))
  1))
(>=
 (appendAll2#1 (nil))
 (+ (nil) 1))
(>=
 (appendAll3 (var @l))
 (+ (appendAll3#1 (var @l)) 1))
(>=
 (appendAll3#1
  (:: (var @l1) (var @ls)))
 (+
  (append
   (appendAll2 (var @l1))
   (appendAll3 (var @ls)))
  1))
(>=
 (appendAll3#1 (nil))
 (+ (nil) 1))
(>=
 (:: (var _x1) (var _x2))
 (var _x2))
(>=
 (append (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>=
 (append#1 (var _x1) (var _x2))
 0)
(>= (appendAll (var _x1)) 0)
(>= (appendAll#1 (var _x1)) 0)
(>= (appendAll2 (var _x1)) 0)
(>= (appendAll2#1 (var _x1)) 0)
(>= (appendAll3 (var _x1)) 0)
(>= (appendAll3#1 (var _x1)) 0)
(>= (nil) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (:: (var _x1) (var _x2)))
(>= (_f2) (nil))
