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
ghci GUBS> let cs = [ f (x + 1) :>=: g (f x), g x :>=: 2 + x ]
~~~~

Currently, GUBS supports solving such constraints via a reduction to SMT, using either 
[minismt](http://cl-informatik.uibk.ac.at/software/minismt/) or 
[z3](https://github.com/Z3Prover/z3), various simplification techniques and separate SCC analysis

~~~~
ghci GUBS> (r,_) <- cs `solveWith` smt Z3 smtOpts
ghci GUBS> r
 Sat (Inter ..)
ghci GUBS> let Just i = interpretation r
ghci GUBS> :module +GUBS.Utils -- exports putDocLn
ghci GUBS GUBS.Utils> putDocLn i
 f(x0) = 2*x0
 g(x0) = 2+x0
~~~~

Here, `smt s o` uses solver `s` to find a model with options `o`. Models are currently restricted to weakly monotone polynomial interpretations,
more precise, function symbols are interpreted as polynomials over the naturals with positive coefficients.


GUBS comes also equipped with an executable that reads constrains from a given filefiles. Constraints are specified as S-Expressions, according to the following rules:

~~~~
SYSTEM     = CONSTRAINT SYSTEM | CONSTRAINT
CONSTRAINT = (>= TERM TERM) | (= TERM TERM)
TERM       = (var IDENT)           -- variable 
           | (+ TERM TERM)         -- addition
           | (max TERM TERM)       -- maximum
           | (* TERM TERM)         -- multiplication
           | (IDENT TERM ... TERM) -- uninterpreted function
           | NATURAL               -- constant
~~~~
where identities `IDENT` are composed of alpha-numerical characters and special symbols `'`, `_`, `/`, `#`, and `?`.
See the [example folder](https://github.com/mzini/gubs/tree/master/examples) in the source distribution for some examples.

~~~~
$ gubs/examples> cat ugo1.cs 
 (>= (f (var x)) 
     (g (h (var x))))
 (>= (h 0) 1)    
 (>= (h (+ (var x) 1)) 
     (f (var x)))
 (>= (g (var x))
     (+ 1 (var x)))

$ gubs/examples> gubs ugo1.cs
 SUCCESS
 f(x0) = 2+x0
 g(x0) = 1+x0
 h(x0) = 1+x0
~~~~
