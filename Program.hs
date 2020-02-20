{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module StaxRomana.Program where

import StaxRomana.Memory (Head)
import StaxRomana.Roman
import AbLib.Control.Parser
import AbLib.Control.ParserUtils
import Data.Maybe (catMaybes)
import Data.Char (isSpace)


data Program = Command :> Program | End
data Command = Pass | Numeral Roman | Command Char | Loop LoopType Program
data LoopType = If | While | Repeat


instance Show Program where
   show End = ""
   show (c :> p) = show c <> show p

instance Show Command where
   show (Command c) = [c]
   show (Numeral r) = show r ++ " "
   show (Loop t q) = case t of
      If     -> "(" ++ show q ++ ")"
      While  -> "{" ++ show q ++ "}"
      Repeat -> "[" ++ show q ++ "]"

instance Semigroup Program where
   End <> q = q
   (c :> p) <> q = c :> (p <> q)

instance Monoid Program where
   mempty = End


program :: [Command] -> Program
program = foldMap (:> End)

nextCmd :: Program -> Head -> (Maybe Command, Program)
nextCmd (Loop t q :> p) h = flip nextCmd h $ case t of
   If     -> if h /= 0 then (q <> p) else p
   While  -> program [Loop If $ program [Loop Repeat q]] <> p
   Repeat -> q <> program [Loop While q] <> p
nextCmd (c :> p) _ = (Just c, p)
nextCmd End _ = (Nothing, End)


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