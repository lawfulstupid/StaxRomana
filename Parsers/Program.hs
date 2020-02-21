module StaxRomana.Parsers.Program where

import StaxRomana.Data.Program
import StaxRomana.Parsers.Roman

import AbLib.Control.Parser
import AbLib.Control.ParserUtils

import Data.Maybe (catMaybes)
import Data.Char (isSpace)

instance Parse Program where
   parser = do
      prog <- many $ peek next >>= \case
         '`' -> comment >> return Nothing
         '(' -> Just <$> loopIf
         '{' -> Just <$> loopWhile
         '[' -> Just <$> loopRepeat
         c | isRoman c -> Just <$> numeral
           | isSpace c -> whitespace >> return Nothing
         _   -> Just <$> command
      return $ program $ catMaybes prog
      where
      
      command :: Parser Command
      command = Command <$> next
      
      isRoman :: Char -> Bool
      isRoman c = elem c "IVXLCDM"
      
      numeral :: Parser Command
      numeral = do
         r <- parser
         eof <|> peek (matchIf (not . isRoman) >> return ())
         return $ Numeral r
      
      loopIf :: Parser Command
      loopIf = do
         match '('
         prog <- parser
         match ')'
         return $ Loop If prog
      
      loopWhile :: Parser Command
      loopWhile = do
         match '{'
         prog <- parser
         match '}'
         return $ Loop While prog
      
      loopRepeat :: Parser Command
      loopRepeat = do
         match '['
         prog <- parser
         match ']'
         return $ Loop Repeat prog
      
      comment :: Parser String
      comment = do
         match '`'
         s <- many $ matchIf $ not . (=='`')
         match '`'
         return s