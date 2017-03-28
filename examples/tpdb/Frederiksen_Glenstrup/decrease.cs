(>=
 (decrease
  (Cons (var x) (var xs)))
 (+ (decrease (var xs)) 1))
(>=
 (decrease (Nil))
 (+ (number42 (Nil)) 1))
(>=
 (goal (var x))
 (+ (decrease (var x)) 1))
(>=
 (number42 (var x))
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
 (Cons (var _x1) (var _x2))
 0)
(>= (Nil) 0)
(>= (decrease (var _x1)) 0)
(>= (goal (var _x1)) 0)
(>= (number42 (var _x1)) 0)
(>=
 (+
  (+ (var _x1) (var _x2))
  (_f1))
 (Cons (var _x1) (var _x2)))
(>= (_f2) (Nil))
