(>= (f41 (var x2)) (var x2))
(>= (f42 (var x21)) (var x21))
(>=
 (f3 (var x21) (f43 (var x2)))
 (f1
  (f41 (var x2))
  (f42 (var x21))))
(>= (f44 (var x3)) (var x3))
(>=
 (f2 (var x2) (var x3))
 (f4
  (f43 (var x2))
  (f44 (var x3))))
(>= (f45 (var x8)) (var x8))
(>= (f46 (var x6)) (var x6))
(>=
 (f47 (var x6) (var x8))
 (f1
  (f45 (var x8))
  (f46 (var x6))))
(>=
 (f1 (+ (var x8) 1) (var x6))
 (+ (f47 (var x6) (var x8)) 1))
(>= (f1 0 (var x11)) (var x11))
(>= (f48 (var x15)) (var x15))
(>=
 (f49 (var x14) (var x15))
 (f3 (f48 (var x15)) (var x14)))
(>=
 (f4 (var x14) (var x15))
 (f3
  (f49 (var x14) (var x15))
  (var x14)))