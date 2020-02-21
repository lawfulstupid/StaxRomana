module StaxRomana.Exception.Runtime where

import GHC.Exception


data RuntimeException
   = InvalidCommand Char


instance Exception RuntimeException

instance Show RuntimeException where
   show (InvalidCommand c) = "Unknown command: " ++ [c]
