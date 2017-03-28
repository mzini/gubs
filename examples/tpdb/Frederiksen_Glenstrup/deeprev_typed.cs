(>=
 (deeprev (C (var x1) (var x2)))
 (+
  (deeprevapp
   (C (var x1) (var x2))
   (N))
  1))
(>= (deeprev (N)) (+ (N) 1))
(>=
 (deeprev (V (var n)))
 (+ (V (var n)) 1))
(>=
 (deeprevapp
  (C (var x1) (var x2))
  (var rest))
 (+
  (deeprevapp
   (var x2)
   (C (var x1) (var rest)))
  1))
(>=
 (deeprevapp (N) (var rest))
 (+ (var rest) 1))
(>=
 (deeprevapp
  (V (var n))
  (var rest))
 (+
  (revconsapp
   (var rest)
   (V (var n)))
  1))
(>=
 (first (C (var x1) (var x2)))
 (+ (var x1) 1))
(>=
 (first (V (var n)))
 (+ (N) 1))
(>=
 (goal (var x))
 (+ (deeprev (var x)) 1))
(>=
 (isEmptyT
  (C (var x1) (var x2)))
 (+ (False) 1))
(>= (isEmptyT (N)) (+ (True) 1))
(>=
 (isEmptyT (V (var n)))
 (+ (False) 1))
(>=
 (isNotEmptyT
  (C (var x1) (var x2)))
 (+ (True) 1))
(>=
 (isNotEmptyT (N))
 (+ (False) 1))
(>=
 (isNotEmptyT (V (var n)))
 (+ (False) 1))
(>=
 (isVal (C (var x1) (var x2)))
 (+ (False) 1))
(>= (isVal (N)) (+ (False) 1))
(>=
 (isVal (V (var n)))
 (+ (True) 1))
(>=
 (revconsapp
  (C (var x1) (var x2))
  (var r))
 (+
  (revconsapp
   (var x2)
   (C (var x1) (var r)))
  1))
(>=
 (revconsapp (N) (var r))
 (+ (var r) 1))
(>=
 (revconsapp
  (V (var n))
  (var r))
 (+ (var r) 1))
(>=
 (second (C (var x1) (var x2)))
 (+ (var x2) 1))
(>=
 (second (V (var n)))
 (+ (N) 1))
(>= (C (var _x1) (var _x2)) 0)
(>= (False) 0)
(>= (N) 0)
(>= (True) 0)
(>= (V (var _x1)) 0)
(>= (deeprev (var _x1)) 0)
(>=
 (deeprevapp
  (var _x1)
  (var _x2))
 0)
(>= (first (var _x1)) 0)
(>= (goal (var _x1)) 0)
(>= (isEmptyT (var _x1)) 0)
(>= (isNotEmptyT (var _x1)) 0)
(>= (isVal (var _x1)) 0)
(>=
 (revconsapp
  (var _x1)
  (var _x2))
 0)
(>= (second (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (C (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (N))
(>= (_f4) (True))
(>=
 (+ (var _x1) (_f5))
 (V (var _x1)))
