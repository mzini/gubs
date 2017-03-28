(>=
 (revapp
  (Cons (var x) (var xs))
  (var rest))
 (+
  (revapp
   (var xs)
   (Cons (var x) (var rest)))
  1))
(>=
 (revapp (Nil) (var rest))
 (+ (var rest) 1))
(>=
 (select
  (Cons (var x) (var xs)))
 (+
  (selects
   (var x)
   (Nil)
   (var xs))
  1))
(>= (select (Nil)) (+ (Nil) 1))
(>=
 (selects
  (var x)
  (var revprefix)
  (Nil))
 (+
  (Cons
   (Cons
    (var x)
    (revapp (var revprefix) (Nil)))
   (Nil))
  1))
(>=
 (selects
  (var x')
  (var revprefix)
  (Cons (var x) (var xs)))
 (+
  (Cons
   (Cons
    (var x')
    (revapp
     (var revprefix)
     (Cons (var x) (var xs))))
   (selects
    (var x)
    (Cons (var x') (var revprefix))
    (var xs)))
  1))
(>=
 (Cons (var _x1) (var _x2))
 (+ (var _x1) (var _x2)))
(>= (Nil) 0)
(>=
 (revapp (var _x1) (var _x2))
 0)
(>= (select (var _x1)) 0)
(>=
 (selects
  (var _x1)
  (var _x2)
  (var _x3))
 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
