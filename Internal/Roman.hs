{-# LANGUAGE LambdaCase #-}

module StaxRomana.Internal.Roman where

import Data.Function ((&))
import AbLib.Data.Tuple

data Roman = Roman {value :: Integer}
   deriving (Eq, Ord)


instance Bounded Roman where
   maxBound = Roman 3999
   minBound = Roman 0

instance Show Roman where
   show (Roman 0) = "nulla"
   show (Roman r) = digits r
      & zip [0..] 
      & reverse 
      & map (msnd showDigit) 
      & foldMap (uncurry raiseOrder)
      where
      
      digits :: Integral a => a -> [a]
      digits 0 = []
      digits n = (n `mod` 10) : digits (n `div` 10)
      
      showDigit :: Integral a => a -> String
      showDigit = \case
         { 0 -> "";  1 -> "I";  2 -> "II";  3 -> "III";  4 -> "IV"
         ; 5 -> "V"; 6 -> "VI"; 7 -> "VII"; 8 -> "VIII"; 9 -> "IX"}

      raiseOrder :: Int -> String -> String
      raiseOrder 0 = id
      raiseOrder n = raiseOrder (n-1) . \s -> flip map s $ \case 
            { 'I' -> 'X'; 'X' -> 'C'; 'C' -> 'M'
            ; 'V' -> 'L'; 'L' -> 'D';  _  -> '?' }