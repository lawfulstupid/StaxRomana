{-# LANGUAGE LambdaCase #-}

module StaxRomana.Parsers.Program where

import StaxRomana.Data.Program
import StaxRomana.Parsers.Roman
import StaxRomana.Exception.Parse

import AbLib.Control.Parser
import AbLib.Control.ParserUtils

import Control.Exception
import Data.Maybe (catMaybes)
import Data.Char (isSpace)

instance Parse Program where
   parser = do
      prog <- many $ peek next >>= \case
         '`' -> onFail (nada comment)     $ throw $ IncompleteComment
         '(' -> onFail (just loopIf)      $ throw $ UnclosedLoop If
         '{' -> onFail (just loopWhile)   $ throw $ UnclosedLoop While
         '[' -> onFail (just loopRepeat)  $ throw $ UnclosedLoop Repeat
         c | isRoman c -> onFail (just numeral) throwInvalidNumeral
           | isSpace c -> nada whitespace
         _   -> onFail (just command) throwUnopenedLoop
      return $ program $ catMaybes prog
      where
      
      nada = (const Nothing <$>)
      just = (Just <$>)
      
      command :: Parser Command
      command = Command <$> matchIf (not . flip elem ")}]")
      
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
      
      throwInvalidNumeral :: Parser a
      throwInvalidNumeral = do
         badnum <- greedy $ many $ matchIf isRoman
         throw $ InvalidNumeral badnum
      
      throwUnopenedLoop :: Parser a
      throwUnopenedLoop = do
         c <- next
         case c of
            ')' -> throw $ UnopenedLoop If
            '}' -> throw $ UnopenedLoop While
            ']' -> throw $ UnopenedLoop Repeat