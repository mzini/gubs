module GUBS.CS (
  -- * Terms
  Term (..)
  , funs
  , funsDL
  , args
  , argsDL
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
  ) where

import           Control.Monad.IO.Class
import           Control.Monad (join)
import           Data.Char (digitToInt)
import           Data.List (nub,foldl')
import           Data.Graph
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.Parsec
import           Text.ParserCombinators.Parsec (CharParser)
import           GUBS.Utils

data Term f v =
  Var v
  | Const Integer
  | Fun f [Term f v]
  | Mult (Term f v) (Term f v)
  | Plus (Term f v) (Term f v)
  | Minus (Term f v) (Term f v)
  | Neg (Term f v)
  deriving (Show)

argsDL :: Term f v -> [Term f v] -> [Term f v]
argsDL Var {} = id
argsDL Const {} = id            
argsDL (Fun f ts) = (++) ts . foldr ((.) . argsDL) id ts
argsDL (Mult t1 t2) = argsDL t1 . argsDL t2 
argsDL (Plus t1 t2) = argsDL t1 . argsDL t2 
argsDL (Minus t1 t2) = argsDL t1 . argsDL t2        
argsDL (Neg t) = argsDL t
                       
args :: Term f v -> [Term f v]       
args = flip argsDL []
       
funsDL :: Term f v -> [f] -> [f]
funsDL Var {} = id
funsDL Const {} = id            
funsDL (Fun f ts) = (f:) . foldr ((.) . funsDL) id ts
funsDL (Mult t1 t2) = funsDL t1 . funsDL t2 
funsDL (Plus t1 t2) = funsDL t1 . funsDL t2 
funsDL (Minus t1 t2) = funsDL t1 . funsDL t2        
funsDL (Neg t) = funsDL t



funs :: Eq f => Term f v -> [f]
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

instance Num (Term f v) where
  fromInteger = Const
  (+) = plus
  (-) = minus
  (*) = mult
  negate = neg
  abs = error "Term: abs undefined"
  signum = error "Term: signum undefined"


data Constraint f v = Term f v :>=: Term f v

lhs, rhs :: Constraint f v -> Term f v 
lhs (l :>=: r) = l
rhs (l :>=: r) = r

type ConstraintSystem f v = [Constraint f v]

sccs :: Eq f => ConstraintSystem f v -> [[Constraint f v]]
sccs cs = map flattenSCC sccs' where
  sccs' = stronglyConnComp [ (c, i , succs c ) | (i,c) <- ecs ]
  ecs = zip [0..] cs
  succs c = [ j | (j,c') <- ecs
                , any (`elem` (funs (rhs c))) (funs (lhs c'))
                  || any (`elem` (funs (lhs c))) (funs (lhs c')) ]

lhss,rhss :: ConstraintSystem f v -> [Term f v]
lhss = map lhs
rhss = map rhs


-- pretty printing
----------------------------------------------------------------------

instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (Term f v) where
   pretty (Var v) = ppCall "var" [PP.pretty v]
   pretty (Const i) = PP.integer i
   pretty (Fun f ts) = ppSexp (PP.pretty f : [PP.pretty ti | ti <- ts])
   pretty (Mult t1 t2) = ppCall "*" [t1,t2]
   pretty (Plus t1 t2) = ppCall "+" [t1,t2]
   pretty (Minus t1 t2) = ppCall "-" [t1,t2]
   pretty (Neg t) = ppCall "neg" [t]

instance (PP.Pretty f, PP.Pretty v) => PP.Pretty (Constraint f v) where
  pretty (l :>=: r) = PP.pretty l PP.<+> PP.text "≥" PP.<+> PP.pretty r

instance {-# OVERLAPPING #-} (PP.Pretty f, PP.Pretty v) => PP.Pretty (ConstraintSystem f v) where
  pretty = PP.vcat . map PP.pretty
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
constraint = parens (literal ">=" >> (:>=:) <$> lexeme term <*> lexeme term)

constraintSystem :: Parser (ConstraintSystem Symbol Variable)
constraintSystem = many (lexeme constraint)

csFromFile :: MonadIO m => FilePath -> m (Either ParseError (ConstraintSystem Symbol Variable))
csFromFile file = runParser parse () sn <$> liftIO (readFile file) where
  sn = "<file " ++ file ++ ">"
  parse = many (whiteSpace1) *> constraintSystem <* eof
