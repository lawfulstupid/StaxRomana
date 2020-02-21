{-# LANGUAGE NoImplicitPrelude #-}

module StaxRomana.Data.Memory (
   module StaxRomana.Data.Memory,
   module StaxRomana.Data.Roman,
   module StaxRomana.Data.Stack
) where

import Prelude hiding (head)
import StaxRomana.Data.Roman
import StaxRomana.Data.Stack


type Memory = Stack Roman
type Head = Roman


head :: Memory -> Head
head Empty = 0
head (_:<x) = x
