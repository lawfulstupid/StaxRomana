{-# LANGUAGE BangPatterns, LambdaCase #-}

module Main where

import Prelude hiding (head)

import StaxRomana.Data.Program
import StaxRomana.Parsers.Program
import StaxRomana.Data.Memory
import StaxRomana.Data.Roman
import StaxRomana.Exception.Runtime

import AbLib.Control.Parser
import AbLib.Data.Tuple

import Control.Exception (throw)
import Data.List (intercalate)
import GHC.IO (unsafePerformIO)
import System.Environment (getArgs)


main :: IO ()
main = do
   args <- getArgs
   if length args == 0 then help else case args !! 0 of
      "help" -> help
      "eval" -> eval
      "load" -> load
      _      -> putStrLn ("Unrecognised command: " ++ args !! 0)

help :: IO ()
help = do
   putStrLn "S T A X   R O M A N A"
   putStrLn ""
   putStrLn "You have two options:"
   putStrLn "eval <code>\tEvaluate program at command line."
   putStrLn "load <file>\tLoad a program from a file."
   putStrLn ""
   putStrLn "github.com/lawfulstupid/StaxRomana"

eval :: IO ()
eval = do
   (_:args) <- getArgs
   compile $ intercalate " " args

load :: IO ()
load = do
   (_:args) <- getArgs
   if length args == 0
   then putStrLn "You need to supply a file to load!"
   else readFile (args !! 0) >>= compile

compile :: String -> IO ()
compile = run . parse

run :: Program -> IO ()
run = flip runWithMemory Empty

runWithMemory :: Program -> Memory -> IO ()
runWithMemory prog !mem = do
   let h = head mem
   let (mcmd, prog') = nextCmd prog h
   case mcmd of
      Just (Numeral r) -> runWithMemory prog' (mem :< r)
      Just (Command c) -> resolve mem c >>= runWithMemory prog'
      Nothing -> if null mem
         then putStr "\n"
         else putStr "\nFinal Memory: " >> print mem

resolve :: Memory -> Char -> IO Memory
resolve mem = return . ($mem) . \case
   {- ARITHMETIC -}
   '+' -> binary (+)
   '-' -> binary (-)
   '*' -> binary (*)
   '/' -> binary div
   '%' -> binary mod
   'S' -> pure . sum . toList
   'P' -> pure . product . toList
   {- COMPARATOR -}
   '=' -> binary $ (xEnum.) . (==)
   '!' -> binary $ (xEnum.) . (/=)
   '<' -> binary $ (xEnum.) . (>)
   '>' -> binary $ (xEnum.) . (<)
   {- LOGIC -}
   '¬' -> unary $ xEnum . (==0)
   '&' -> binary (*)
   '|' -> binary $ \x y -> abs x + abs y
   {- STACK MANIPULATION -}
   '.' -> snd . pop
   '_' -> const Empty
   'd' -> direct 1 $ \[x]     -> [x,x]
   '?' -> direct 2 $ \[x,y]   -> [x,y,x,y]
   '$' -> direct 2 $ \[x,y]   -> [y,x]
   ';' -> direct 3 $ \[x,y,z] -> [z,x,y]
   ':' -> \case {Empty -> Empty; (s :< a) -> pure a <> s}
   'r' -> fromList . reverse . toList
   {- I/O -}
   '#' -> sideEffect $ (\(x,s) -> print x >> return s) . pop
   '~' -> sideEffect $ (\s -> print s >> return Empty) . toList
   '\''-> sideEffect $ (\(x,s) -> putStr [xEnum x] >> return s) . pop
   '"' -> sideEffect $ (\s -> putStr s >> return Empty) . map xEnum . toList
   ',' -> sideEffect $ \s -> flip push s . xEnum <$> getChar
   '@' -> sideEffect $ \s -> flip pushn s . map xEnum <$> getLine
   err -> throw $ InvalidCommand err
   where
   
   xEnum :: (Enum a, Enum b) => a -> b
   xEnum = toEnum . fromEnum
   
   unary :: (Roman -> Roman) -> Memory -> Memory
   unary f = direct 1 $ \[x] -> [f x]
   
   binary :: (Roman -> Roman -> Roman) -> Memory -> Memory
   binary f = direct 2 $ \[x,y] -> [f x y]
   
   direct :: Int -> ([Roman] -> [Roman]) -> Memory -> Memory
   direct n f = uncurry pushn . mfst f . popn n
   
   sideEffect :: (Memory -> IO Memory) -> Memory -> Memory
   sideEffect action = unsafePerformIO . action
