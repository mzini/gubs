(>=
 (goal (var x))
 (+ (list (var x)) 1))
(>=
 (list (Cons (var x) (var xs)))
 (+ (list (var xs)) 1))
(>= (list (Nil)) (+ (True) 1))
(>=
 (list (Nil))
 (+ (isEmpty[Match] (Nil)) 1))
(>=
 (notEmpty
  (Cons (var x) (var xs)))
 (+ (True) 1))
(>=
 (notEmpty (Nil))
 (+ (False) 1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>= (goal (var _x1)) 0)
(>=
 (isEmpty[Match] (var _x1))
 0)
(>= (list (var _x1)) 0)
(>= (notEmpty (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
(>=
 (+ (var _x1) (_f5))
 (isEmpty[Match] (var _x1)))
