{-# LANGUAGE TypeOperators #-}
module Web.Channel where

import           Control.Coroutine.Monadic (Repeat, Eps, (:?:), (:!:), (:&:))
import qualified Data.ByteString.Lazy       as BSL
-- import qualified Data.ByteString.Lazy.Char8 as BSL8
import           Data.Change               (Changing, Changes)
import qualified Data.ID                    as ID
import           Data.List                 (intercalate)

-- Idea: how to force that all channels used by the client are served by the
-- server:
-- • create a stack-like data structure of channels in the common part;
-- • require the client side to get channels from that stack;
-- • export only a function to pop the stack by accepting a token that
-- proves that that channel has been established;
-- • require the empty stack when starting the server.

type ChannelID
  = [Int]

-- type Tag
--   = String

showLabel :: ChannelID -> String
showLabel = intercalate "." . map show

data Channel s
  = Channel ChannelID

data File
  = File BSL.ByteString

-- Interest/push/pull mechanism, for objects of type a identified by type i.

-- type Interest i a
--   = Repeat
--     ( (i :?: a :!: Eps) -- Announce interest; receive current object.
--       :&:
--       (i :?:       Eps) -- Drop interest.
--     )

type PullChange a
  = Repeat (Changes a :!: Eps)

type PushChange a
  = Repeat (Changes a :?: Eps)

-- data InterestPushPull i a
--
-- interest :: Token (InterestPushPull i a) -> Channel (Interest i a)
-- interest (Token l) = Channel $ l ++ [0]

-- pullChange :: Token (InterestPushPull i a) -> Channel (PullChange a)
-- pullChange (Token l) = Channel $ l ++ [1]

-- pushChange :: Token (InterestPushPull i a) -> Channel (PushChange a)
-- pushChange (Token l) = Channel $ l ++ [2]

-- Initial/push/pull mechanism for objects of type a.

type Initial a
  = Repeat (a :!: Eps)

data InitialPushPull a

initial :: Token (InitialPushPull a) -> Channel (Initial a)
initial (Token l) = Channel $ l ++ [0]

pullChange :: Token (InitialPushPull a) -> Channel (PullChange a)
pullChange (Token l) = Channel $ l ++ [1]

pushChange :: Token (InitialPushPull a) -> Channel (PushChange a)
pushChange (Token l) = Channel $ l ++ [2]

-- ID

type CreateID a
  = Repeat (a :?: ID.ID a :!: Eps)

type LookupID a
  = Repeat (ID.ID a :?: Maybe (ID.WithID a) :!: Eps)

type UpdateID a
  = Repeat (ID.WithID a :?: Eps)

type DeleteID a
  = Repeat (ID.ID a :?: Eps)

type MarkDeletedID a
  = Repeat (ID.ID a :?: Eps)

newtype Token s
  = Token ChannelID

data CRUD a

-- list :: Token (CRUD a) -> Channel (List a)
-- list (Token l) = Channel $ l ++ [0]

-- listIDs :: Token (CRUD a) -> Channel (ListIDs a)
-- listIDs (Token l) = Channel $ l ++ [1]

createID :: Token (CRUD a) -> Channel (CreateID a)
createID (Token l) = Channel $ l ++ [2]

lookupID :: Token (CRUD a) -> Channel (LookupID a)
lookupID (Token l) = Channel $ l ++ [3]

updateID :: Token (CRUD a) -> Channel (UpdateID a)
updateID (Token l) = Channel $ l ++ [4]

deleteID :: Token (CRUD a) -> Channel (DeleteID a)
deleteID (Token l) = Channel $ l ++ [5]

markDeletedID :: Token (CRUD a) -> Channel (MarkDeletedID a)
markDeletedID (Token l) = Channel $ l ++ [6]

-- interestID :: Token (CRUD a) -> Channel (Interest (ID.ID a))
-- interestID (Token l) = Channel $ l ++ [7]

-- pullChangeID :: Token (CRUD a) -> Channel (PullChange (ID.WithID a))
-- pullChangeID (Token l) = Channel $ l ++ [8]

