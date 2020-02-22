module StaxRomana.Exception.RomanArithmetic where

import StaxRomana.Internal.Roman

import GHC.Exception


data RomanNumeralException
   = BadAddition Roman Roman
   | BadSubtraction Roman Roman
   | BadMultiplication Roman Roman
   | BadDivision
   | ExceedsMaximum
   | ExceedsMinimum

instance Exception RomanNumeralException

instance Show RomanNumeralException where
   show = \case
      BadAddition x y         -> "non potest addunt " ++ show x ++ " et " ++ show y
      BadSubtraction x y      -> "non potest subtrahere " ++ show y ++ " ex " ++ show x
      BadMultiplication x y   -> "non potest multiplicamini " ++ show x ++ " et " ++ show y
      BadDivision             -> "non potest dividere per nulla"
      ExceedsMaximum          -> "Romani numeralis non potest esse supra " ++ show (maxBound :: Roman)
      ExceedsMinimum          -> "Romani numeralis non potest esse infra " ++ show (minBound :: Roman)
