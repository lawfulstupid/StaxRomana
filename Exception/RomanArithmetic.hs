module StaxRomana.Exception.RomanArithmetic where

import StaxRomana.Internal.Roman

import GHC.Exception


data RomanNumeralException
   = BadAddition Roman Roman
   | BadSubtraction Roman Roman
   | BadMultiplication Roman Roman
   | ExceedsMaximum
   | ExceedsMinimum

instance Exception RomanNumeralException

instance Show RomanNumeralException where
   show = \case
      BadSubtraction x y      -> "Cannot subtract " ++ show y ++ " from " ++ show x
      BadAddition x y         -> "Cannot add " ++ show y ++ " to " ++ show x
      BadMultiplication x y   -> "Cannot multiply " ++ show x ++ " by " ++ show y
      ExceedsMaximum          -> "Roman numerals cannot exceed " ++ show (maxBound :: Roman)
      ExceedsMinimum          -> "Roman numerals cannot lie below " ++ show (minBound :: Roman)
