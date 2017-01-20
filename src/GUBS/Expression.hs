module GUBS.Expression
       (
         Expression
       , variable
       , zero
       , toNatural
       , evalWithM
       , evalWith
       ) where

import           GUBS.Utils
import           GUBS.Polynomial
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type Expression v = Polynomial v Integer

zero :: Expression v
zero = zeroPoly
