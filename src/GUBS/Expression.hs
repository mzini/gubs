module GUBS.Expression
       (
         Expression
       , variable
       , constant
       , evalWithM
       , evalWith
       , toNum
       ) where

import           GUBS.Utils
import           GUBS.Polynomial
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type Expression v = Polynomial v Integer
