(>=
 (compS_f#1
  (compS_f (var x2))
  (var x1))
 (+
  (compS_f#1
   (var x2)
   (S (var x1)))
  1))
(>=
 (compS_f#1 (id) (var x3))
 (+ (S (var x3)) 1))
(>= (iter#3 (0)) (+ (id) 1))
(>=
 (iter#3 (S (var x6)))
 (+
  (compS_f (iter#3 (var x6)))
  1))
(>= (main (0)) (+ (0) 1))
(>=
 (main (S (var x9)))
 (+
  (compS_f#1
   (iter#3 (var x9))
   (0))
  1))
(>= (0) 0)
(>= (S (var _x1)) 0)
(>=
 (compS_f (var _x1))
 (var _x1))
(>=
 (compS_f#1 (var _x1) (var _x2))
 (var _x1))
(>= (id) 0)
(>= (iter#3 (var _x1)) 0)
(>= (main (var _x1)) 0)
(>= (_f1) (0))
(>=
 (+ (var _x1) (_f2))
 (S (var _x1)))
(>=
 (+ (var _x1) (_f3))
 (compS_f (var _x1)))
(>= (_f4) (id))
