(>=
 (add0 (0) (var x2))
 (+ (var x2) 1))
(>=
 (add0 (S (var x)) (var x2))
 (+
  (+
   (S (0))
   (add0 (var x2) (var x)))
  1))
(>=
 (+ (var x) (S (0)))
 (S (var x)))
(>=
 (+ (S (0)) (var y))
 (S (var y)))
(>=
 (+ (var _x1) (var _x2))
 (var _x2))
(>= (0) 0)
(>= (S (var _x1)) 0)
(>=
 (add0 (var _x1) (var _x2))
 0)
(>= (_f1) (0))
(>=
 (+ (var _x1) (_f2))
 (S (var _x1)))
