{-# LANGUAGE TypeOperators #-}
module Web.Channel where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ID              as ID
import           Data.List                 (intercalate)
import           Control.Coroutine.Monadic (Repeat, Eps, (:?:), (:!:), (:&:))

-- Idea: how to force that all channels used by the client are served by the
-- server:
-- • create a stack-like data structure of channels in the common part;
-- • require the client side to get channels from that stack;
-- • export only a function to pop the stack by accepting a token that
-- proves that that channel has been established;
-- • require the empty stack when starting the server.

type ChannelID
  = [Int]

showLabel :: ChannelID -> String
showLabel = intercalate "." . map show

data Channel s
  = Channel ChannelID

data File
  = File BSL.ByteString

-- data Channels
--   = Channels [Channel]

-- add :: Channel s -> Channels -> Channels
-- add c


type List a
  = Repeat ([ID.WithID a] :!: Eps)

type Create a
  = Repeat (a :?: (ID.ID a :!: Eps))

type Update a
  = Repeat (ID.WithID a :?: Eps)

type Delete a
  = Repeat (ID.ID a :?: Eps)

type MarkDeleted a
  = Repeat (ID.ID a :?: Eps)

newtype Token s
  = Token ChannelID

list :: Token (CRUD a) -> Channel (List a)
list (Token l) = Channel $ l ++ [0]

create :: Token (CRUD a) -> Channel (Create a)
create (Token l) = Channel $ l ++ [1]

update :: Token (CRUD a) -> Channel (Update a)
update (Token l) = Channel $ l ++ [2]

delete :: Token (CRUD a) -> Channel (Delete a)
delete (Token l) = Channel $ l ++ [3]

markDeleted :: Token (CRUD a) -> Channel (MarkDeleted a)
markDeleted (Token l) = Channel $ l ++ [4]

data CRUD a

-- crud :: Channel (CRUD a)
