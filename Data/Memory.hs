module StaxRomana.Data.Memory (
   module StaxRomana.Data.Memory,
   module StaxRomana.Internal.Data,
   module StaxRomana.Data.Stack
) where

import Prelude hiding (head)
import StaxRomana.Internal.Data (Memory(..), Head(..))
import StaxRomana.Data.Stack
import StaxRomana.Data.Roman


head :: Memory -> Head
head Empty = 0
head (_:<x) = x
