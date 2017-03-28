(>=
 (foldl#3
  (var x16)
  (Cons (var x24) (var x6)))
 (+
  (foldl#3
   (Cons (var x24) (var x16))
   (var x6))
  1))
(>=
 (foldl#3 (var x2) (Nil))
 (+ (var x2) 1))
(>=
 (main (var x1))
 (+ (foldl#3 (Nil) (var x1)) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>=
 (foldl#3 (var _x1) (var _x2))
 0)
(>= (main (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
