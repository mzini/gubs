(>=
 (+Full (0) (var y))
 (+ (var y) 1))
(>=
 (+Full (S (var x)) (var y))
 (+
  (+Full (var x) (S (var y)))
  1))
(>=
 (f (var x))
 (+ (* (var x) (var x)) 1))
(>=
 (goal (var xs))
 (+ (map (var xs)) 1))
(>=
 (map (Cons (var x) (var xs)))
 (+
  (Cons
   (f (var x))
   (map (var xs)))
  1))
(>= (map (Nil)) (+ (Nil) 1))
(>= (* (var x) (0)) (0))
(>= (* (var x) (S (0))) (var x))
(>=
 (* (var x) (S (S (var y))))
 (+
  (var x)
  (* (var x) (S (var y)))))
(>= (* (0) (var y)) (0))
(>= (* (var _x1) (var _x2)) 0)
(>=
 (+ (var _x1) (var _x2))
 (var _x2))
(>=
 (+Full (var _x1) (var _x2))
 0)
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (f (var _x1)) 0)
(>= (goal (var _x1)) 0)
(>= (map (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (+ (var _x1) (var _x2)))
(>= (_f2) (0))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f3))
 (Cons (var _x1) (var _x2)))
(>= (_f4) (Nil))
(>=
 (+ (var _x1) (_f5))
 (S (var _x1)))
