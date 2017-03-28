(>=
 (dec
  (Cons
   (Cons (var x) (var xs))
   (Nil)))
 (+ (dec (Nil)) 1))
(>=
 (dec
  (Cons
   (Cons (var x') (var xs'))
   (Cons (var x) (var xs))))
 (+
  (dec (Cons (var x) (var xs)))
  1))
(>=
 (dec
  (Cons
   (Nil)
   (Cons (var x) (var xs))))
 (+
  (dec (Cons (var x) (var xs)))
  1))
(>=
 (dec (Cons (Nil) (Nil)))
 (+ (Nil) 1))
(>=
 (goal (var x))
 (+ (nestdec (var x)) 1))
(>=
 (isNilNil
  (Cons
   (Cons (var x) (var xs))
   (Nil)))
 (+ (False) 1))
(>=
 (isNilNil
  (Cons
   (Cons (var x') (var xs'))
   (Cons (var x) (var xs))))
 (+ (False) 1))
(>=
 (isNilNil
  (Cons
   (Nil)
   (Cons (var x) (var xs))))
 (+ (False) 1))
(>=
 (isNilNil (Cons (Nil) (Nil)))
 (+ (True) 1))
(>=
 (nestdec
  (Cons (var x) (var xs)))
 (+
  (nestdec
   (dec (Cons (var x) (var xs))))
  1))
(>=
 (nestdec (Nil))
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
                   (Nil))))))))))))))))))
  1))
(>=
 (number17 (var n))
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
                   (Nil))))))))))))))))))
  1))
(>=
 (Cons (var _x1) (var _x2))
 0)
(>= (False) 0)
(>= (Nil) 0)
(>= (True) 0)
(>= (dec (var _x1)) 0)
(>= (goal (var _x1)) 0)
(>= (isNilNil (var _x1)) 0)
(>=
 (nestdec (var _x1))
 (var _x1))
(>= (number17 (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (False))
(>= (_f3) (Nil))
(>= (_f4) (True))
