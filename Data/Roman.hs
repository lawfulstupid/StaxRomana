{-# LANGUAGE LambdaCase #-}

module StaxRomana.Data.Roman (
   module StaxRomana.Data.Roman,
   module StaxRomana.Internal.Roman
) where

import StaxRomana.Internal.Roman
import StaxRomana.Exception.RomanArithmetic
import AbLib.Data.Tuple
import Control.Exception (throw)


instance Num Roman where
   x + y = if r > maxBound then throw $ BadAddition x y else r where r = Roman (toInteger x + toInteger y)
   x - y = if r < minBound then throw $ BadSubtraction x y else r where r = Roman (toInteger x - toInteger y)
   x * y = if r > maxBound then throw $ BadMultiplication x y else r where r = Roman (toInteger x * toInteger y)
   abs = id
   signum (Roman r) = Roman $ signum r
   fromInteger n = if r > maxBound then throw ExceedsMaximum
      else if r < minBound then throw ExceedsMinimum
      else r where r = Roman $ fromInteger n

instance Real Roman where
   toRational (Roman x) = toRational x

instance Enum Roman where
   toEnum = fromInteger . toEnum
   fromEnum = fromEnum . toInteger

instance Integral Roman where
   quotRem _ 0 = throw BadDivision
   quotRem (Roman x) (Roman y) = Roman $# quotRem x y
   divMod _ 0 = throw BadDivision
   divMod (Roman x) (Roman y) = Roman $# divMod x y
   toInteger (Roman x) = toInteger x
