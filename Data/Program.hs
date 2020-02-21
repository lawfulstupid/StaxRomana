module StaxRomana.Data.Program (
   module StaxRomana.Data.Program,
   module StaxRomana.Internal.Data
) where

import StaxRomana.Internal.Data (Program(..), Command(..), LoopType(..))
import StaxRomana.Data.Memory (Head)
import StaxRomana.Data.Roman


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
