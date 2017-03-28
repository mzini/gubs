(>= (f36 (var x4)) (var x4))
(>=
 (f37 (var x4))
 (f36 (var x4)))
(>=
 (f38 (var x4))
 (f37 (var x4)))
(>= (f39 (var x6)) (var x6))
(>=
 (f40 (var x4))
 (f38 (var x4)))
(>= (f41 (var x3)) (var x3))
(>=
 (f42
  (var x3)
  (var x4)
  (var x6))
 (+
  (f3
   (f39 (var x6))
   (f41 (var x3))
   (f42
    (var x3)
    (var x4)
    (var x6)))
  (f40 (var x4))))
(>=
 (f43 (var x3) (var x6))
 (f1
  (f39 (var x6))
  (f41 (var x3))))
(>=
 (f44
  (var x3)
  (var x4)
  (var x6))
 (+
  (f2
   (f39 (var x6))
   (f41 (var x3)))
  (f42
   (var x3)
   (var x4)
   (var x6))))
(>=
 (f45
  (var x3)
  (var x4)
  (var x6))
 (f44
  (var x3)
  (var x4)
  (var x6)))
(>=
 (f1 (+ (var x6) 1) (var x3))
 (+ (f43 (var x3) (var x6)) 1))
(>=
 (+
  (f2 (+ (var x6) 1) (var x3))
  (var x4))
 (+
  (f45
   (var x3)
   (var x4)
   (var x6))
  1))
(>= (f46 (var x10)) (var x10))
(>= (f1 0 (var x9)) (var x9))
(>=
 (+ (f2 0 (var x9)) (var x10))
 (+ (f46 (var x10)) 1))