(>=
 (eqNatList
  (Cons (var x) (var xs))
  (Cons (var y) (var ys)))
 (+
  (eqNatList[Match][Cons][Match][Cons][Ite]
   (!EQ (var x) (var y))
   (var y)
   (var ys)
   (var x)
   (var xs))
  1))
(>=
 (eqNatList
  (Cons (var x) (var xs))
  (Nil))
 (+ (False) 1))
(>=
 (eqNatList
  (Nil)
  (Cons (var y) (var ys)))
 (+ (False) 1))
(>=
 (eqNatList (Nil) (Nil))
 (+ (True) 1))
(>=
 (goal
  (var a1)
  (var b1)
  (var a2)
  (var b2)
  (var a3)
  (var b3))
 (+
  (nolexicord
   (var a1)
   (var b1)
   (var a2)
   (var b2)
   (var a3)
   (var b3))
  1))
(>=
 (nolexicord
  (Cons (var x) (var xs))
  (var b1)
  (var a2)
  (var b2)
  (var a3)
  (var b3))
 (+
  (nolexicord[Ite][False][Ite]
   (eqNatList
    (Cons (var x) (var xs))
    (var b1))
   (Cons (var x) (var xs))
   (var b1)
   (var a2)
   (var b2)
   (var a3)
   (var b3))
  1))
(>=
 (nolexicord
  (Nil)
  (var b1)
  (var a2)
  (var b2)
  (var a3)
  (var b3))
 (+
  (Cons
   (Nil)
   (Cons
    (Nil)
    (Cons
     (Nil)
     (Cons
      (Nil)
      (Cons
       (Nil)
       (Cons
        (Nil)
        (Cons
         (Nil)
         (Cons
          (Nil)
          (Cons
           (Nil)
           (Cons
            (Nil)
            (Cons
             (Nil)
             (Cons
              (Nil)
              (Cons
               (Nil)
               (Cons
                (Nil)
                (Cons
                 (Nil)
                 (Cons
                  (Nil)
                  (Cons
                   (Nil)
                   (Cons
                    (Nil)
                    (Cons
                     (Nil)
                     (Cons
                      (Nil)
                      (Cons
                       (Nil)
                       (Cons
                        (Nil)
                        (Cons
                         (Nil)
                         (Cons
                          (Nil)
                          (Cons
                           (Nil)
                           (Cons
                            (Nil)
                            (Cons
                             (Nil)
                             (Cons
                              (Nil)
                              (Cons
                               (Nil)
                               (Cons
                                (Nil)
                                (Cons
                                 (Nil)
                                 (Cons
                                  (Nil)
                                  (Cons
                                   (Nil)
                                   (Cons
                                    (Nil)
                                    (Cons
                                     (Nil)
                                     (Cons
                                      (Nil)
                                      (Cons
                                       (Nil)
                                       (Cons
                                        (Nil)
                                        (Cons
                                         (Nil)
                                         (Cons
                                          (Nil)
                                          (Cons
                                           (Nil)
                                           (Cons
                                            (Nil)
                                            (Nil)))))))))))))))))))))))))))))))))))))))))))
  1))
(>=
 (number42)
 (+
  (Cons
   (Nil)
   (Cons
    (Nil)
    (Cons
     (Nil)
     (Cons
      (Nil)
      (Cons
       (Nil)
       (Cons
        (Nil)
        (Cons
         (Nil)
         (Cons
          (Nil)
          (Cons
           (Nil)
           (Cons
            (Nil)
            (Cons
             (Nil)
             (Cons
              (Nil)
              (Cons
               (Nil)
               (Cons
                (Nil)
                (Cons
                 (Nil)
                 (Cons
                  (Nil)
                  (Cons
                   (Nil)
                   (Cons
                    (Nil)
                    (Cons
                     (Nil)
                     (Cons
                      (Nil)
                      (Cons
                       (Nil)
                       (Cons
                        (Nil)
                        (Cons
                         (Nil)
                         (Cons
                          (Nil)
                          (Cons
                           (Nil)
                           (Cons
                            (Nil)
                            (Cons
                             (Nil)
                             (Cons
                              (Nil)
                              (Cons
                               (Nil)
                               (Cons
                                (Nil)
                                (Cons
                                 (Nil)
                                 (Cons
                                  (Nil)
                                  (Cons
                                   (Nil)
                                   (Cons
                                    (Nil)
                                    (Cons
                                     (Nil)
                                     (Cons
                                      (Nil)
                                      (Cons
                                       (Nil)
                                       (Cons
                                        (Nil)
                                        (Cons
                                         (Nil)
                                         (Cons
                                          (Nil)
                                          (Cons
                                           (Nil)
                                           (Cons
                                            (Nil)
                                            (Nil)))))))))))))))))))))))))))))))))))))))))))
  1))
(>= (!EQ (0) (0)) (True))
(>=
 (!EQ (0) (S (var y)))
 (False))
(>=
 (!EQ (S (var x)) (0))
 (False))
(>=
 (!EQ (S (var x)) (S (var y)))
 (!EQ (var x) (var y)))
(>=
 (nolexicord[Ite][False][Ite]
  (False)
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs)))
 (nolexicord
  (var xs')
  (var xs')
  (var xs')
  (var xs')
  (var xs')
  (var xs)))
(>=
 (nolexicord[Ite][False][Ite]
  (True)
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x') (var xs'))
  (Cons (var x) (var xs))
  (Cons (var x') (var xs')))
 (nolexicord
  (var xs')
  (var xs')
  (var xs')
  (var xs')
  (var xs')
  (var xs)))
(>= (!EQ (var _x1) (var _x2)) 0)
(>= (0) 0)
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (S (var _x1)) 0)
(>= (True) 0)
(>=
 (eqNatList (var _x1) (var _x2))
 0)
(>=
 (eqNatList[Match][Cons][Match][Cons][Ite]
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5))
 (var _x1))
(>=
 (goal
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (nolexicord
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6))
 0)
(>=
 (nolexicord[Ite][False][Ite]
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)
  (var _x6)
  (var _x7))
 (var _x1))
(>= (number42) 0)
(>= (_f1) (0))
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f2))
 (Cons (var _x1) (var _x2)))
(>= (_f3) (False))
(>= (_f4) (Nil))
(>=
 (+ (var _x1) (_f5))
 (S (var _x1)))
(>= (_f6) (True))
(>=
 (+
  (+
   (var _x1)
   (+
    (var _x2)
    (+
     (var _x3)
     (+ (var _x4) (var _x5)))))
  (_f7))
 (eqNatList[Match][Cons][Match][Cons][Ite]
  (var _x1)
  (var _x2)
  (var _x3)
  (var _x4)
  (var _x5)))
