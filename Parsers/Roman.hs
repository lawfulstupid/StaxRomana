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
      guard (r > 0) -- if all parsers pass
      return $ fromInteger r
      where

      count :: Parser String -> Parser Integer
      count f = fromIntegral . length <$> f
            
      milia :: Parser Integer
      milia = do
         m <- count $ between (0,3) $ match 'M'
         guard (m <= 3)
         return (1000 * m)

      centum :: Parser Integer
      centum = matchAs "CD" 400 <|> matchAs "CM" 900 <|> do
         d <- count $ between (0,1) $ match 'D'
         c <- count $ between (0,3) $ match 'C'
         return (500 * d + 100 * c)

      decem :: Parser Integer
      decem = matchAs "XL" 40 <|> matchAs "XC" 90 <|> do
         l <- count $ between (0,1) $ match 'L'
         x <- count $ between (0,3) $ match 'X'
         return (50 * l + 10 * x)

      unus :: Parser Integer
      unus = matchAs "IV" 4 <|> matchAs "IX" 9 <|> do
         v <- count $ between (0,1) $ match 'V'
         i <- count $ between (0,3) $ match 'I'
         return (5 * v + i)
