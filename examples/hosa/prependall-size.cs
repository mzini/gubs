(>= (f71 (var x2)) (var x2))
(>= (f72 (var x34)) (var x34))
(>=
 (f2 (var x34) (f73 (var x2)))
 (f1
  (f71 (var x2))
  (f72 (var x34))))
(>= (f74 (var x4)) (var x4))
(>= (f75 (var x3)) (var x3))
(>=
 (f6 (var x2) (var x3) (var x4))
 (f4
  (f73 (var x2))
  (f75 (var x3))
  (f74 (var x4))))
(>=
 (f5 (var x2) (var x3) (var x4))
 (f3
  (f73 (var x2))
  (f75 (var x3))
  (f74 (var x4))))
(>= (f76 (var x9)) (var x9))
(>= (f77 (var x7)) (var x7))
(>=
 (f78 (var x7) (var x9))
 (f1
  (f76 (var x9))
  (f77 (var x7))))
(>=
 (f1 (+ (var x9) 1) (var x7))
 (+ (f78 (var x7) (var x9)) 1))
(>= (f1 0 (var x12)) (var x12))
(>= (f79 (var x16)) (var x16))
(>=
 (f80
  (var x15)
  (var x16)
  (var x19)
  (var x63))
 (f2 (f79 (var x16)) (var x15)))
(>= (f81 (var x63)) (var x63))
(>=
 (f2 (var x63) (f82 (var x15)))
 (f2 (f81 (var x63)) (var x15)))
(>= (f83 (var x19)) (var x19))
(>= (f84 (var x16)) (var x16))
(>=
 (f85
  (var x15)
  (var x16)
  (var x19)
  (var x63))
 (f4
  (f82 (var x15))
  (f84 (var x16))
  (f83 (var x19))))
(>=
 (f80
  (var x15)
  (var x16)
  (var x19)
  (var x63))
 (f3
  (f82 (var x15))
  (f84 (var x16))
  (f83 (var x19))))
(>=
 (f4
  (var x15)
  (var x16)
  (+ (var x19) 1))
 (+
  (f85
   (var x15)
   (var x16)
   (var x19)
   (var x63))
  1))
(>=
 (f3
  (var x15)
  (var x16)
  (+ (var x19) 1))
 (f80
  (var x15)
  (var x16)
  (var x19)
  (var x63)))
(>=
 (f4 (var x22) (var x23) 0)
 0)
(>=
 (f3 (var x22) (var x23) 0)
 (f86 ()))