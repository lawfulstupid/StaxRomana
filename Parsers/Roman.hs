module StaxRomana.Parsers.Roman where

import StaxRomana.Data.Roman
import AbLib.Control.Parser


instance Read Roman where
   readsPrec _ = parseS

instance Parse Roman where
   parser = do
      m <- milia
      c <- centum
      x <- decem
      i <- unus
      let r = m + c + x + i
      guard (r > 0)
      return r
      where

      roman :: Parser String -> Parser Roman
      roman f = Roman . fromIntegral . length <$> f
            
      milia :: Parser Roman
      milia = do
         m <- roman $ between (0,3) $ match 'M'
         guard (m <= 3)
         return (1000 * m)

      centum :: Parser Roman
      centum = matchAs "CD" 400 <|> matchAs "CM" 900 <|> do
         d <- roman $ between (0,1) $ match 'D'
         c <- roman $ between (0,3) $ match 'C'
         return (500 * d + 100 * c)

      decem :: Parser Roman
      decem = matchAs "XL" 40 <|> matchAs "XC" 90 <|> do
         l <- roman $ between (0,1) $ match 'L'
         x <- roman $ between (0,3) $ match 'X'
         return (50 * l + 10 * x)

      unus :: Parser Roman
      unus = matchAs "IV" 4 <|> matchAs "IX" 9 <|> do
         v <- roman $ between (0,1) $ match 'V'
         i <- roman $ between (0,3) $ match 'I'
         return (5 * v + i)
