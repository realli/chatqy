module Qy.Error.Room where

import GHC.Generics
import Servant
import Data.CaseInsensitive (mk)
import Data.Aeson
import Data.Text
import Qy.Error

data RoomOpErr = RoomOpErr {
      status :: Int
    , message :: Text
    } deriving (Show, Generic)

instance ToJSON RoomOpErr


roomNotExists :: RoomOpErr
roomNotExists = RoomOpErr 50101 "No Such Room"

roomAlreadyExists :: RoomOpErr
roomAlreadyExists = RoomOpErr 50102 "Room Already Exists"

roomNameNotMatchBody :: RoomOpErr
roomNameNotMatchBody = RoomOpErr 50103 "Room Name not match the body"

instance ToServantErr RoomOpErr where
    toServantErr es = err404 { errBody = encode es
                             , errHeaders = [(mk "Content-Type", "application/json")]
                             }

