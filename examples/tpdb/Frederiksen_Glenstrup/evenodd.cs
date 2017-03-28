(>=
 (even (Cons (var x) (var xs)))
 (+ (odd (var xs)) 1))
(>= (even (Nil)) (+ (True) 1))
(>=
 (evenodd (var x))
 (+ (even (var x)) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (odd (Cons (var x) (var xs)))
 (+ (even (var xs)) 1))
(>= (odd (Nil)) (+ (False) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>= (even (var _x1)) 0)
(>= (evenodd (var _x1)) 0)
(>= (notEmpty (var _x1)) 0)
(>= (odd (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
