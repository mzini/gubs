module GUBS.Constraint where

import           GUBS.Utils
import qualified Text.PrettyPrint.ANSI.Leijen as PP


infix 4 :>=:
data Constraint a = a :>=: a deriving (Show, Functor)

data ConditionalConstraint a = CC { premises :: [Constraint a]
                                  , constraint :: Constraint a }
  deriving (Show, Functor)

lhs, rhs :: Constraint a -> a
lhs (a :>=: _) = a
rhs (_ :>=: b) = b


instance PP.Pretty a => PP.Pretty (Constraint a) where
  pretty (l :>=: r) = PP.pretty l PP.</> PP.text "≥" PP.<+> PP.pretty r
  -- pretty (l :=: r)  = PP.pretty l PP.</> PP.text "=" PP.<+> PP.pretty r

instance PrettySexp a => PrettySexp (Constraint a) where
  prettySexp (l :>=: r) = ppCall "≥" [prettySexp l, prettySexp r]
  -- prettySexp (l :=: r)  = PP.pretty l PP.</> PP.text "=" PP.<+> PP.pretty r
