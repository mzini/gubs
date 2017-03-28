(>=
 (bool2Nat (False))
 (+ (0) 1))
(>=
 (bool2Nat (True))
 (+ (S (0)) 1))
(>=
 (e1
  (var a)
  (var b)
  (var res)
  (var t))
 (+
  (e2
   (var a)
   (var b)
   (var res)
   (< (var a) (var b)))
  1))
(>=
 (e2
  (var a)
  (var b)
  (var res)
  (False))
 (+ (False) 1))
(>=
 (e2
  (var a)
  (var b)
  (var res)
  (True))
 (+
  (e3
   (var a)
   (var b)
   (var res)
   (True))
  1))
(>=
 (e3
  (var a)
  (var b)
  (var res)
  (var t))
 (+
  (e4
   (var a)
   (var b)
   (var res)
   (< (var b) (var a)))
  1))
(>=
 (e4
  (var a)
  (var b)
  (var res)
  (False))
 (+ (False) 1))
(>=
 (e4
  (var a)
  (var b)
  (var res)
  (True))
 (+ (True) 1))
(>=
 (e5
  (var a)
  (var b)
  (var res)
  (var t))
 (+ (True) 1))
(>=
 (e6
  (var a)
  (var b)
  (var res)
  (var t))
 (+ (False) 1))
(>=
 (e7
  (var a)
  (var b)
  (var res)
  (var t))
 (+ (False) 1))
(>=
 (e8
  (var a)
  (var b)
  (var res)
  (var t))
 (+ (var res) 1))
(>=
 (equal0 (var a) (var b))
 (+
  (e1
   (var a)
   (var b)
   (False)
   (False))
  1))
(>=
 (gcd (var x) (var y))
 (+
  (l1
   (var x)
   (var y)
   (0)
   (False)
   (False)
   (False))
  1))
(>= (help1 (0)) (+ (False) 1))
(>=
 (help1 (S (0)))
 (+ (False) 1))
(>=
 (help1 (S (S (var x))))
 (+ (True) 1))
(>=
 (l1
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l2
   (var x)
   (var y)
   (var res)
   (var tmp)
   (var mtmp)
   (False))
  1))
(>=
 (l10
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l11
   (var x)
   (var y)
   (var res)
   (var tmp)
   (var mtmp)
   (< (var x) (var y)))
  1))
(>=
 (l11
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (False))
 (+
  (l14
   (var x)
   (var y)
   (var res)
   (var tmp)
   (var mtmp)
   (False))
  1))
(>=
 (l11
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (True))
 (+
  (l12
   (var x)
   (var y)
   (var res)
   (var tmp)
   (var mtmp)
   (True))
  1))
(>=
 (l12
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l13
   (var x)
   (var y)
   (var res)
   (var tmp)
   (monus (var x) (var y))
   (var t))
  1))
(>=
 (l13
  (var x)
  (var y)
  (var res)
  (var tmp)
  (False)
  (var t))
 (+
  (l16
   (var x)
   (var y)
   (gcd (0) (var y))
   (var tmp)
   (False)
   (var t))
  1))
(>=
 (l13
  (var x)
  (var y)
  (var res)
  (var tmp)
  (True)
  (var t))
 (+
  (l16
   (var x)
   (var y)
   (gcd (S (0)) (var y))
   (var tmp)
   (True)
   (var t))
  1))
(>=
 (l14
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l15
   (var x)
   (var y)
   (var res)
   (var tmp)
   (monus (var x) (var y))
   (var t))
  1))
(>=
 (l15
  (var x)
  (var y)
  (var res)
  (var tmp)
  (False)
  (var t))
 (+
  (l16
   (var x)
   (var y)
   (gcd (var y) (0))
   (var tmp)
   (False)
   (var t))
  1))
(>=
 (l15
  (var x)
  (var y)
  (var res)
  (var tmp)
  (True)
  (var t))
 (+
  (l16
   (var x)
   (var y)
   (gcd (var y) (S (0)))
   (var tmp)
   (True)
   (var t))
  1))
(>=
 (l16
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+ (var res) 1))
(>=
 (l2
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (False))
 (+
  (l3
   (var x)
   (var y)
   (var res)
   (var tmp)
   (var mtmp)
   (False))
  1))
(>=
 (l2
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (True))
 (+ (var res) 1))
(>=
 (l3
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l4
   (var x)
   (var y)
   (0)
   (var tmp)
   (var mtmp)
   (var t))
  1))
(>=
 (l4
  (var x')
  (var x)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l5
   (var x')
   (var x)
   (var res)
   (var tmp)
   (var mtmp)
   (False))
  1))
(>=
 (l5
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (False))
 (+
  (l7
   (var x)
   (var y)
   (var res)
   (var tmp)
   (var mtmp)
   (False))
  1))
(>=
 (l5
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (True))
 (+ (0) 1))
(>=
 (l6
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+ (0) 1))
(>=
 (l7
  (var x)
  (var y)
  (var res)
  (var tmp)
  (var mtmp)
  (var t))
 (+
  (l8
   (var x)
   (var y)
   (var res)
   (equal0 (var x) (var y))
   (var mtmp)
   (var t))
  1))
(>=
 (l8
  (var res)
  (var y)
  (var res')
  (True)
  (var mtmp)
  (var t))
 (+ (var res) 1))
(>=
 (l8
  (var x)
  (var y)
  (var res)
  (False)
  (var mtmp)
  (var t))
 (+
  (l10
   (var x)
   (var y)
   (var res)
   (False)
   (var mtmp)
   (var t))
  1))
(>=
 (l9
  (var res)
  (var y)
  (var res')
  (var tmp)
  (var mtmp)
  (var t))
 (+ (var res) 1))
(>=
 (m1
  (var a)
  (var x)
  (var res)
  (var t))
 (+
  (m2
   (var a)
   (var x)
   (var res)
   (False))
  1))
(>=
 (m2
  (var a)
  (var b)
  (var res)
  (False))
 (+
  (m4
   (var a)
   (var b)
   (var res)
   (False))
  1))
(>=
 (m2
  (0)
  (var b)
  (var res)
  (True))
 (+ (False) 1))
(>=
 (m2
  (S (0))
  (var b)
  (var res)
  (True))
 (+ (False) 1))
(>=
 (m2
  (S (S (var x)))
  (var b)
  (var res)
  (True))
 (+ (True) 1))
(>=
 (m3
  (0)
  (var b)
  (var res)
  (var t))
 (+ (False) 1))
(>=
 (m3
  (S (0))
  (var b)
  (var res)
  (var t))
 (+ (False) 1))
(>=
 (m3
  (S (S (var x)))
  (var b)
  (var res)
  (var t))
 (+ (True) 1))
(>=
 (m4
  (S (var x'))
  (S (var x))
  (var res)
  (var t))
 (+
  (m5
   (S (var x'))
   (S (var x))
   (monus (var x') (var x))
   (var t))
  1))
(>=
 (m5
  (var a)
  (var b)
  (var res)
  (var t))
 (+ (var res) 1))
(>=
 (monus (var a) (var b))
 (+
  (m1
   (var a)
   (var b)
   (False)
   (False))
  1))
(>= (< (var x) (0)) (False))
(>= (< (0) (S (var y))) (True))
(>=
 (< (S (var x)) (S (var y)))
 (< (var x) (var y)))
(>= (0) 0)
(>= (< (var _x1) (var _x2)) 0)
(>= (False) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>= (bool2Nat (var _x1)) 0)
(>=
 (e1
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (e2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 (var _x4))
(>=
 (e3
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (e4
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 (var _x4))
(>=
 (e5
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (e6
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (e7
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (e8
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (equal0 (var _x1) (var _x2))
 0)
(>= (gcd (var _x1) (var _x2)) 0)
(>= (help1 (var _x1)) 0)
(>=
 (l1
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l10
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l11
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 (var _x6))
(>=
 (l12
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l13
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 (var _x5))
(>=
 (l14
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l15
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 (var _x5))
(>=
 (l16
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 (var _x3))
(>=
 (l2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l3
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l4
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l5
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l6
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l7
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (l8
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 (var _x4))
(>=
 (l9
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (m1
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (m2
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (m3
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (m4
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 0)
(>=
 (m5
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4))
 (var _x3))
(>=
 (monus (var _x1) (var _x2))
 0)
(>= (_f1) (0))
(>= (_f2) (False))
(>=
 (+ (var _x1) (_f3))
 (S (var _x1)))
(>= (_f4) (True))
