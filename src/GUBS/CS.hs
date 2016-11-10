module GUBS.CS (
  -- * Terms
  Term (..)
  , funs
  , funsDL
  , vars
  , varsDL
  , args
  , argsDL
  , substitute
    -- * Constraints
  , Constraint (..)
  , lhs
  , rhs
  , definedSymbol
    -- * Constraint Systems
  , ConstraintSystem (..)
  , lhss
  , rhss
  , sccs
    -- * Parsing
  , Symbol (..)
  , Variable (..)
  , csFromFile
  , csToFile
  ) where

import           Control.Monad.IO.Class
import           Control.Monad (join)
import           System.IO 
import           Data.Char (digitToInt)
import           Data.List (nub,foldl')
import           Data.String
import           Data.Graph
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Parsec
import           Text.ParserCombinators.Parsec (CharParser)
import           GUBS.Utils

data Term f v = Var v
              | Const Integer
              | Fun f [Term f v]
              | Mult (Term f v) (Term f v)
              | Plus (Term f v) (Term f v)
              | Minus (Term f v) (Term f v)
              | Neg (Term f v)
    deriving (Show)

instance IsString (Term f String) where fromString = Var

argsDL :: Term f v -> [Term f v] -> [Term f v]
argsDL Var{} = id
argsDL Const{} = id            
argsDL (Fun f ts) = (++) ts . foldr ((.) . argsDL) id ts
argsDL (Mult t1 t2) = argsDL t1 . argsDL t2 
argsDL (Plus t1 t2) = argsDL t1 . argsDL t2 
argsDL (Minus t1 t2) = argsDL t1 . argsDL t2        
argsDL (Neg t) = argsDL t
                       
args :: Term f v -> [Term f v]       
args = flip argsDL []

varsDL :: Term f v -> [v] -> [v]
varsDL (Var v) = (v:)
varsDL Const {} = id            
varsDL (Fun f ts) = foldr ((.) . varsDL) id ts
varsDL (Mult t1 t2) = varsDL t1 . varsDL t2 
varsDL (Plus t1 t2) = varsDL t1 . varsDL t2 
varsDL (Minus t1 t2) = varsDL t1 . varsDL t2        
varsDL (Neg t) = varsDL t


vars :: Term f v -> [v]
vars = flip varsDL []
        
funsDL :: Term f v -> [(f,Int)] -> [(f,Int)]
funsDL Var {} = id
funsDL Const {} = id            
funsDL (Fun f ts) = ((f,length ts):) . foldr ((.) . funsDL) id ts
funsDL (Mult t1 t2) = funsDL t1 . funsDL t2 
funsDL (Plus t1 t2) = funsDL t1 . funsDL t2 
funsDL (Minus t1 t2) = funsDL t1 . funsDL t2        
funsDL (Neg t) = funsDL t

funs :: Eq f => Term f v -> [(f,Int)]
funs = nub . flip funsDL []

definedSymbol :: Term f v -> Maybe f
definedSymbol (Fun f _) = Just f
definedSymbol _ = Nothing              


liftIntOp f _ (Const i) (Const j) = Const (f i j)
liftIntOp _ c e1 e2 = c e1 e2

plus :: Term f v -> Term f v -> Term f v
plus (Const 0) y = y
plus x (Const 0) = x
plus x y = liftIntOp (+) Plus x y

mult :: Term f v -> Term f v -> Term f v
mult (Const 0) _ = Const 0
mult x (Const 0) = Const 0
mult (Const 1) y = y
mult x (Const 1) = x
mult x y = liftIntOp (*) Mult x y

minus :: Term f v -> Term f v -> Term f v
minus x (Const 0) = x
minus x y = liftIntOp (-) Minus x y

neg :: Term f v -> Term f v
neg (Neg i) = i
neg (Const j) = Const (-j)
neg e = Neg e

substitute :: Eq v => (v -> Term f v') -> Term f v -> Term f v'
substitute subst (Var v)       = subst v
substitute _     (Const c)     = Const c
substitute subst (Fun f ts)    = Fun f (substitute subst `map` ts)
substitute subst (Mult t1 t2)  = substitute subst t1 * substitute subst t2
substitute subst (Plus t1 t2)  = substitute subst t1 + substitute subst t2
substitute subst (Minus t1 t2) = substitute subst t1 - substitute subst t2
substitute subst (Neg t)       = neg (substitute subst t)


instance Num (Term f v) where
  fromInteger = Const
  (+) = plus
  (-) = minus
  (*) = mult
  negate = neg
  abs = error "Term: abs undefined"
  signum = error "Term: signum undefined"


data Constraint f v = Term f v :=: Term f v
                    | Term f v :>=: Term f v
  deriving (Show)

infixl 1 :>=:
infixl 1 :=:

lhs, rhs :: Constraint f v -> Term f v
lhs (l :=: r) = l
lhs (l :>=: r) = l
rhs (l :>=: r) = r
rhs (l :=: r) = r

cfuns :: Eq f => Constraint f c -> [(f,Int)]
cfuns c = nub (funsDL (lhs c) (funsDL (rhs c) []))

type ConstraintSystem f v = [Constraint f v]

--TODO check if that makes sense
sccs :: Eq f => ConstraintSystem f v -> [[Constraint f v]]
sccs cs = map flattenSCC sccs'
  where
    sccs' = stronglyConnComp [ (c, i, succs c) | (i, c) <- ecs ]
    ecs = zip [0 ..] cs
    succs c@(l :=: r) = succs (l :>=: r) ++ succs (r :>=: l)
    succs c@(l :>=: r) = [ j
                         | (j, c') <- ecs
                         , any (`elem` (cfuns c)) (funs (lhs c')) ] --TODO account for equality constraints
                         -- any (`elem` cfuns c') (cfuns c)

lhss,rhss :: ConstraintSystem f v -> [Term f v]
lhss = map lhs
rhss = map rhs


-- pretty printing
----------------------------------------------------------------------

ppBin :: (PP.Pretty a, PP.Pretty b) => (PP.Doc -> PP.Doc) -> String -> a -> b -> PP.Doc
ppBin par f a b = par (PP.pretty a PP.</> PP.text f PP.<+> PP.pretty b)

instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (Term f v) where
  pretty = pp id where
    pp _ (Var v) = PP.pretty v
    pp _ (Const i) = PP.integer i
    pp _ (Fun f ts) = PP.pretty f PP.<> PP.tupled [PP.pretty ti | ti <- ts]
    pp par (Mult t1 t2) = ppBin par "*" t1 t2
    pp par (Plus t1 t2) = ppBin par "+" t1 t2
    pp par (Minus t1 t2) = ppBin par "-" t1 t2 
    pp par (Neg t) = par (PP.text "neg" PP.<> PP.tupled [PP.pretty t])

instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (Constraint f v) where
  pretty (l :>=: r) = PP.pretty l PP.</> PP.text "≥" PP.<+> PP.pretty r
  pretty (l :=: r)  = PP.pretty l PP.</> PP.text "=" PP.<+> PP.pretty r

instance {-# OVERLAPPING #-} (PP.Pretty f, PP.Pretty v) => PP.Pretty (ConstraintSystem f v) where
  pretty = PP.vcat . map PP.pretty

class PrettySexp a where
  prettySexp :: a -> PP.Doc

instance (PP.Pretty f, PP.Pretty v) => PrettySexp (Term f v) where 
  prettySexp (Var v) = ppCall "var" [PP.pretty v]
  prettySexp (Const i) = PP.integer i
  prettySexp (Fun f []) = ppSexp [PP.pretty f, ppSexp []]
  prettySexp (Fun f ts) = ppSexp (PP.pretty f : [prettySexp ti | ti <- ts])
  prettySexp (Mult t1 t2) = ppCall "*" [t1,t2]
  prettySexp (Plus t1 t2) = ppCall "+" [t1,t2]
  prettySexp (Minus t1 t2) = ppCall "-" [t1,t2]
  prettySexp (Neg t) = ppCall "neg" [t]

instance (PP.Pretty f, PP.Pretty v) => PrettySexp (Constraint f v) where
  prettySexp (l :>=: r) = ppCall "≥" [prettySexp l, prettySexp r]
  prettySexp (l :=: r)  = PP.pretty l PP.</> PP.text "=" PP.<+> PP.pretty r

instance (PP.Pretty f, PP.Pretty v) => PrettySexp (ConstraintSystem f v) where
  prettySexp = PP.vcat . map PP.pretty
-- parsing
----------------------------------------------------------------------

type Parser = CharParser ()

newtype Variable = Variable String deriving (Eq, Ord, Show)
newtype Symbol = Symbol String deriving (Eq, Ord, Show)

instance PP.Pretty Variable where pretty (Variable v) = PP.text v
instance PP.Pretty Symbol where pretty (Symbol v) = PP.text v

whiteSpace1 :: Parser String
whiteSpace1 = many1 ((space <|> tab <|> newline) <?> "whitespace")

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

lexeme :: Parser a -> Parser a
lexeme p = p <* many whiteSpace1

literal :: String -> Parser ()
literal s = lexeme (string s) >> return ()

identifier :: Parser String
identifier = lexeme (many (try alphaNum <|> oneOf "'_/#?*+-"))

natural :: Parser Int
natural = lexeme (foldl' (\a i -> a * 10 + digitToInt i) 0 <$> many1 digit)

term :: Parser (Term Symbol Variable)
term = try constant <|> parens (try var <|> compound) where
  var = literal "var" >> (Var <$> Variable <$> identifier)
  constant = Const <$> toInteger <$> natural
  compound = join (toTerm <$> identifier <*> many (lexeme term))
  toTerm f ts | f `notElem` ["*","+","-","neg"] = return (Fun (Symbol f) ts)
  toTerm "*" [t1,t2] = return (Mult t1 t2)
  toTerm "+" [t1,t2] = return (Plus t1 t2)
  toTerm "-" [t1,t2] = return (Minus t1 t2)
  toTerm "neg" [t] = return (Neg t)
  toTerm f _ = parserFail $ "Unexpected number of arguments for operation '" ++ show f ++ "'"
  
constraint :: Parser (Constraint Symbol Variable)
constraint = parens $ do
  c <- try (literal ">=" >> return (:>=:))
       <|> (literal "=" >> return (:=:))
  c <$> lexeme term <*> lexeme term

constraintSystem :: Parser (ConstraintSystem Symbol Variable)
constraintSystem = many (lexeme constraint)

csFromFile :: MonadIO m => FilePath -> m (Either ParseError (ConstraintSystem Symbol Variable))
csFromFile file = runParser parse () sn <$> liftIO (readFile file) where
  sn = "<file " ++ file ++ ">"
  parse = many (whiteSpace1) *> constraintSystem <* eof

csToFile :: (MonadIO m, PP.Pretty f, PP.Pretty v) => ConstraintSystem f v -> FilePath -> m ()
csToFile cs f = liftIO $ do
   handle <- openFile f WriteMode
   PP.hPutDoc handle (prettySexp cs)
   hClose handle
