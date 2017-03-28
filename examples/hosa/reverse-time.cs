(>= (f55 (var x3)) (var x3))
(>= (f56 (var x2)) (var x2))
(>=
 (f57 (var x3))
 (f55 (var x3)))
(>=
 (f58 (var x2) (var x3))
 (+
  (f3
   (f56 (var x2))
   (f59 ())
   (f60 (var x2) (var x3)))
  (f57 (var x3))))
(>= (f59 ()) 0)
(>=
 (f60 (var x2) (var x3))
 (f58 (var x2) (var x3)))
(>=
 (f61 (var x2) (var x3))
 (+
  (f2 (f56 (var x2)) (f59 ()))
  (f60 (var x2) (var x3))))
(>=
 (f4 (var x2))
 (f1 (f56 (var x2)) (f59 ())))
(>=
 (+ (f5 (var x2)) (var x3))
 (+ (f61 (var x2) (var x3)) 1))
(>= (f62 (var x7)) (var x7))
(>= (f63 (var x9)) (var x9))
(>=
 (f64 (var x7))
 (f62 (var x7)))
(>=
 (f65
  (var x6)
  (var x7)
  (var x9))
 (+
  (f3
   (f63 (var x9))
   (f66 (var x6))
   (f67
    (var x6)
    (var x7)
    (var x9)))
  (f64 (var x7))))
(>=
 (f68
  (var x6)
  (var x7)
  (var x9))
 (f65
  (var x6)
  (var x7)
  (var x9)))
(>= (f69 (var x6)) (var x6))
(>=
 (f70
  (var x6)
  (var x7)
  (var x9))
 (f68
  (var x6)
  (var x7)
  (var x9)))
(>=
 (f66 (var x6))
 (+ (f69 (var x6)) 1))
(>=
 (f67
  (var x6)
  (var x7)
  (var x9))
 (f70
  (var x6)
  (var x7)
  (var x9)))
(>=
 (f71
  (var x6)
  (var x7)
  (var x9))
 (+
  (f2
   (f63 (var x9))
   (f66 (var x6)))
  (f67
   (var x6)
   (var x7)
   (var x9))))
(>=
 (f1 (+ (var x9) 1) (var x6))
 (f1
  (f63 (var x9))
  (f66 (var x6))))
(>=
 (+
  (f2 (+ (var x9) 1) (var x6))
  (var x7))
 (+
  (f71
   (var x6)
   (var x7)
   (var x9))
  1))
(>= (f72 (var x13)) (var x13))
(>= (f1 0 (var x12)) (var x12))
(>=
 (+ (f2 0 (var x12)) (var x13))
 (+ (f72 (var x13)) 1))