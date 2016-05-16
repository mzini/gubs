(= (f (var x))
    (g (h (var x))))
(= (h 0) 1)    
(= (h (+ (var x) 1))
    (f (var x)))
(= (g (var x))
    (* 2 (var x)))

