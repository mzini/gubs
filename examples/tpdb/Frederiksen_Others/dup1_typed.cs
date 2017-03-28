(>=
 (a
  (C (var x1) (var x2))
  (var y))
 (+
  (C
   (a (var x1) (var y))
   (a
    (var x2)
    (C (var x1) (var x2))))
  1))
(>= (a (Z) (var y)) (+ (Z) 1))
(>=
 (eqZList
  (C (var x1) (var x2))
  (C (var y1) (var y2)))
 (+
  (and
   (eqZList (var x1) (var y1))
   (eqZList (var x2) (var y2)))
  1))
(>=
 (eqZList
  (C (var x1) (var x2))
  (Z))
 (+ (False) 1))
(>=
 (eqZList
  (Z)
  (C (var y1) (var y2)))
 (+ (False) 1))
(>=
 (eqZList (Z) (Z))
 (+ (True) 1))
(>=
 (first (C (var x1) (var x2)))
 (+ (var x1) 1))
(>=
 (second (C (var x1) (var x2)))
 (+ (var x2) 1))
(>=
 (and (False) (False))
 (False))
(>=
 (and (False) (True))
 (False))
(>=
 (and (True) (False))
 (False))
(>= (and (True) (True)) (True))
(>=
 (C (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (False) 0)
(>= (True) 0)
(>= (Z) 0)
(>= (a (var _x1) (var _x2)) 0)
(>=
 (and (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>=
 (eqZList (var _x1) (var _x2))
 0)
(>= (first (var _x1)) 0)
(>= (second (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (C (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (True))
(>= (_f4) (Z))
