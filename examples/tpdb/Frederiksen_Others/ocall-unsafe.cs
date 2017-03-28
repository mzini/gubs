(>=
 (dbl (0) (var y))
 (+ (var y) 1))
(>=
 (dbl (S (0)) (S (0)))
 (+ (S (S (S (S (0))))) 1))
(>= (unsafe (0)) (+ (0) 1))
(>=
 (unsafe (S (var x)))
 (+
  (dbl (unsafe (var x)) (0))
  1))
(>= (0) 0)
(>= (S (var _x1)) 0)
(>=
 (dbl (var _x1) (var _x2))
 (var _x1))
(>= (unsafe (var _x1)) 0)
(>= (_f1) (0))
(>=
 (+ (var _x1) (_f2))
 (S (var _x1)))
