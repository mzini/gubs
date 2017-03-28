(>= (even (0)) (+ (S (0)) 1))
(>=
 (even (S (var x)))
 (+ (odd (var x)) 1))
(>= (odd (0)) (+ (0) 1))
(>=
 (odd (S (var x)))
 (+ (even (var x)) 1))
(>= (0) 0)
(>= (S (var _x1)) 0)
(>= (even (var _x1)) 0)
(>= (odd (var _x1)) 0)
(>= (_f1) (0))
(>=
 (+ (var _x1) (_f2))
 (S (var _x1)))
