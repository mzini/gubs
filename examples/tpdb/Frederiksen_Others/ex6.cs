(>=
 (f (var x1) (0))
 (+ (g (var x1) (0)) 1))
(>=
 (f (var y) (S (var x)))
 (+ (f (S (var y)) (var x)) 1))
(>=
 (g (0) (var x2))
 (+ (var x2) 1))
(>=
 (g (S (var x)) (var y))
 (+ (g (var x) (S (var y))) 1))
(>= (0) 0)
(>= (S (var _x1)) 0)
(>= (f (var _x1) (var _x2)) 0)
(>= (g (var _x1) (var _x2)) 0)
(>= (_f1) (0))
(>=
 (+ (var _x1) (_f2))
 (S (var _x1)))
