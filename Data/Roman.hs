{-# LANGUAGE LambdaCase #-}

module StaxRomana.Data.Roman where

import AbLib.Data.Tuple
import Data.Function ((&))


data Roman = Roman {value :: Word}
   deriving (Eq, Ord)


instance Show Roman where
   show (Roman 0) = "nulla"
   show (Roman r) = digits r
      & zip [0..] 
      & reverse 
      & map (msnd showDigit) 
      & foldMap (uncurry raiseOrder)
      where
      
      digits :: Word -> [Word]
      digits 0 = []
      digits n = (n `mod` 10) : digits (n `div` 10)
      
      showDigit :: Word -> String
      showDigit = \case
         { 0 -> "";  1 -> "I";  2 -> "II";  3 -> "III";  4 -> "IV"
         ; 5 -> "V"; 6 -> "VI"; 7 -> "VII"; 8 -> "VIII"; 9 -> "IX"}

      raiseOrder :: Integer -> String -> String
      raiseOrder 0 = id
      raiseOrder n = raiseOrder (n-1) . \s -> flip map s $ \case 
            { 'I' -> 'X'; 'X' -> 'C'; 'C' -> 'M'
            ; 'V' -> 'L'; 'L' -> 'D';  _  -> '?' }

instance Num Roman where
   Roman x + Roman y = Roman (x + y)
   Roman x - Roman y = Roman (x - y)
   Roman x * Roman y = Roman (x * y)
   abs (Roman x)     = Roman $ abs x
   signum (Roman x)  = Roman $ signum x
   fromInteger n     = Roman $ fromInteger n

instance Real Roman where
   toRational (Roman x) = toRational x

instance Enum Roman where
   toEnum n = Roman $ toEnum n
   fromEnum (Roman x) = fromEnum x

instance Integral Roman where
   quotRem (Roman x) (Roman y) = Roman $# quotRem x y
   toInteger (Roman x) = toInteger x

instance Bounded Roman where
   minBound = Roman minBound
   maxBound = Roman maxBound
