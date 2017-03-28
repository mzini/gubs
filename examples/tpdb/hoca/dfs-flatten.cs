(>=
 (dfsAcc#3
  (Leaf (var x8))
  (var x16))
 (+
  (Cons (var x8) (var x16))
  1))
(>=
 (dfsAcc#3
  (Node (var x6) (var x4))
  (var x2))
 (+
  (dfsAcc#3
   (var x4)
   (dfsAcc#3 (var x6) (var x2)))
  1))
(>=
 (main (var x1))
 (+
  (revApp#2
   (dfsAcc#3 (var x1) (Nil))
   (Nil))
  1))
(>=
 (revApp#2
  (Cons (var x6) (var x4))
  (var x2))
 (+
  (revApp#2
   (var x4)
   (Cons (var x6) (var x2)))
  1))
(>=
 (revApp#2 (Nil) (var x16))
 (+ (var x16) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Leaf (var _x1)) 0)
(>= (Nil) 0)
(>=
 (Node (var _x1) (var _x2))
 0)
(>=
 (dfsAcc#3 (var _x1) (var _x2))
 (var _x2))
(>= (main (var _x1)) 0)
(>=
 (revApp#2 (var _x1) (var _x2))
 (var _x1))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>=
 (+ (var _x1) (_f2))
 (Leaf (var _x1)))
(>= (_f3) (Nil))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f4))
 (Node (var _x1) (var _x2)))
