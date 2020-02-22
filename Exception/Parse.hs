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
      IncompleteComment -> "imperfecta nota"
      UnclosedLoop t    -> "imperfecta repetitio ex " ++ show t
      UnopenedLoop t    -> "nulla repetitio ex " ++ show t ++ " ad perficiendum"
      InvalidNumeral r  -> "irritum Romani numeralis: " ++ r
