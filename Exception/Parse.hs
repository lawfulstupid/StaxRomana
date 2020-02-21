{-# LANGUAGE LambdaCase #-}

module StaxRomana.Exception.Parse where

import StaxRomana.Data.Program
import Control.Exception
import GHC.Exception

data ParseException
   = IncompleteComment
   | UnclosedLoop LoopType
   | UnopenedLoop LoopType
   | InvalidNumeral String


instance Exception ParseException

instance Show ParseException where
   show = \case
      IncompleteComment -> "Incomplete comment"
      UnclosedLoop t    -> "Unclosed " ++ show t ++ " loop"
      UnopenedLoop t    -> "No " ++ show t ++ " loop to close"
      InvalidNumeral r  -> "Invalid Roman numeral: " ++ r
