# GUBS
The acronym GUBS stands for GUBS Upper Bound Solver.

Given a set of (in)equalities over arithmetical expressions and 
uninterpreted functions, GUBS is trying to find a model, 
i.e. interpretation into the naturals.

The module GUBS exports all the essential functionality.
~~~~
ghci> :module +GUBS
~~~~

Let us define a simple constraint system over two unary functions `f` and `g`. 
~~~~
ghci GUBS> let f n = Fun 'f' [n]
ghci GUBS> let g n = Fun 'g' [n]
ghci GUBS> let x = Var 'x'
ghci GUBS> let cs = [ f (x + 1) :>=: g (f x), g x :=: 2 + x ]
~~~~

Currently, GUBS supports solving such constraints via a reduction to SMT, using either 
[minismt](http://cl-informatik.uibk.ac.at/software/minismt/) or 
[z3](https://github.com/Z3Prover/z3).

~~~~
ghci GUBS> (r,_) <- cs `solveWith` smt Z3 1
ghci GUBS> r
 Sat (Inter (fromList [('f',Poly ...),('g',Poly ...)]))
ghci GUBS> Just i <- interpretation r
ghci GUBS> :module +GUBS.Utils -- exports putDocLn
ghci GUBS GUBS.Utils> putDocLn i
 f(x0) = 7+15*x0
 g(x0) = 2+x0
~~~~

Here, `smt s i` uses solver `s` to find a model of degree `i`. Models are currently restricted to weakly monotone polynomial interpretations,
more precise, function symbols are interpreted as polynomials over the naturals with positive coefficients.

Constraints can also be read from files. Constraints are specified as S-Expressions, according to the following rules:

~~~~
SYSTEM     = CONSTRAINT SYSTEM | CONSTRAINT
CONSTRAINT = (>= TERM TERM) | (= TERM TERM)
TERM       = (var IDENT)           -- variable 
           | (+ TERM TERM)         -- addition
           | (+ TERM TERM)         -- substraction
           | (* TERM TERM)         -- multiplication
           | (neg TERM)            -- negation
           | (IDENT TERM ... TERM) -- uninterpreted function
           | INTEGER               -- constant
~~~~
where identities `IDENT` are composed of alpha-numerical characters and special symbols `'`, `_`, `/`, `#`, and `?`.
See the [example folder](https://github.com/mzini/gubs/tree/master/examples) in the source distribution for some examples.
For a minimal executable, see [examples/gubs.hs](https://github.com/mzini/gubs/blob/master/examples/gubs.hs).


~~~~
$ gubs/examples> cat ugo1.cs 
 (= (f (var x)) 
    (g (h (var x))))
 (= (h 0) 1)    
 (= (h (+ (var x) 1)) 
    (f (var x)))
 (= (g (var x))
    (+ 1 (var x)))

$ gubs/examples> runhaskell gubs.hs ugo1.cs 2>/dev/zero
 SUCCESS
 f(x0) = 2+x0
 g(x0) = 1+x0
 h(x0) = 1+x0
~~~~
