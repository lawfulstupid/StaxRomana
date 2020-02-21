module StaxRomana.Internal.Data where

data Stack a = Empty | Stack a :< a

data Program = Command :> Program | End
data Command = Pass | Numeral Roman | Command Char | Loop LoopType Program
data LoopType = If | While | Repeat
   deriving (Eq, Show)

data Roman = Roman {value :: Word}
   deriving (Eq, Ord)

type Memory = Stack Roman
type Head = Roman