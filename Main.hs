{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module StaxRomana.Main where

import Prelude hiding (head)
import StaxRomana.Program
import StaxRomana.Memory
import AbLib.Control.Parser
import System.Environment (getArgs)
import Data.Functor ((<&>))
import AbLib.Data.Tuple
import GHC.IO

todo = errorWithoutStackTrace "Not yet implemented!"


main :: IO ()
main = getArgs <&> concat >>= compile

compile :: String -> IO ()
compile = run . parse

run :: Program -> IO ()
run = flip runWithMemory Empty

runWithMemory :: Program -> Memory -> IO ()
runWithMemory prog !mem = do
   let h = head mem
   let (mcmd, prog') = nextCmd prog h
   case mcmd of
      Nothing -> putStr "\nFinal Memory: " >> print mem
      Just (Numeral r) -> runWithMemory prog' (mem :< r)
      Just (Command c) -> resolve mem c >>= runWithMemory prog'

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
   'r' -> fromList . reverse . toList
   {- I/O -}
   '#' -> sideEffect $ (\(x,s) -> print x >> return s) . pop
   '~' -> sideEffect $ (\xs -> print xs >> return Empty) . toList
   '\''-> sideEffect $ (\(x,s) -> putStr [xEnum x] >> return s) . pop
   '"' -> sideEffect $ (\xs -> putStr xs >> return Empty) . map xEnum . toList
   ',' -> todo
   '@' -> todo

   where
   pushR :: Word -> Memory -> Memory
   pushR = push . Roman
   
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