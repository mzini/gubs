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
 (subtrees (var @t))
 (+ (subtrees#1 (var @t)) 1))
(>=
 (subtrees#1 (leaf))
 (+ (nil) 1))
(>=
 (subtrees#1
  (node
   (var @x)
   (var @t1)
   (var @t2)))
 (+
  (subtrees#2
   (subtrees (var @t1))
   (var @t1)
   (var @t2)
   (var @x))
  1))
(>=
 (subtrees#2
  (var @l1)
  (var @t1)
  (var @t2)
  (var @x))
 (+
  (subtrees#3
   (subtrees (var @t2))
   (var @l1)
   (var @t1)
   (var @t2)
   (var @x))
  1))
(>=
 (subtrees#3
  (var @l2)
  (var @l1)
  (var @t1)
  (var @t2)
  (var @x))
 (+
  (::
   (node
    (var @x)
    (var @t1)
    (var @t2))
   (append (var @l1) (var @l2)))
  1))
(>=
 (:: (var _x1) (var _x2))
 (var _x2))
(>=
 (append (var _x1) (var _x2))
 0)
(>=
 (append#1 (var _x1) (var _x2))
 0)
(>= (leaf) 0)
(>= (nil) 0)
(>=
 (node
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>= (subtrees (var _x1)) 0)
(>= (subtrees#1 (var _x1)) 0)
(>=
 (subtrees#2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 (var _x1))
(>=
 (subtrees#3
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5))
 (var _x1))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (:: (var _x1) (var _x2)))
(>= (_f2) (leaf))
(>= (_f3) (nil))
(>=
 (+
  (+
   (var _x1)
   (+ (var _x2) (var _x3)))
  (_f4))
 (node
  (var _x1)
  (var _x2)
  (var _x3)))
