{-# LANGUAGE FlexibleInstances #-}

module Qy.Chat.Types (
    emptyRoomMap
    , Room(..)
    , Client(..)
    , getRoomChan
    , ErrorMsg(..)
    , IncomingMsg(..)
    , ChanMessage(..)
    , UserName
    , RoomName
    , RoomMap
    )
    where

import Data.Text
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import qualified Data.Set as Set
import Data.HashMap.Strict as M
import qualified Network.WebSockets as WS
import Data.Aeson
import Data.Aeson.Types
import qualified Control.Applicative as A
import Control.Applicative ((<$>), (<*>))
import Data.Function (on)


type UserName = Text
type RoomName = Text
type RoomMap = TVar (HashMap RoomName Room)

emptyRoomMap :: IO RoomMap
emptyRoomMap = newTVarIO M.empty

data Room = Room { roomName :: RoomName
                 , roomClients :: TVar (Set.Set Client)
                 , roomChan :: TChan ChanMessage
                 }

getRoomChan = roomChan

data Client = Client { clientName :: Text
                     , clientRooms :: TVar (Set.Set Room)
                     , clientChan :: TChan ChanMessage
                     , clientRoomChans :: TVar [TChan ChanMessage]
                     }


instance Eq Client where
    (==) = on (==) clientName

instance Ord Client where
    compare = on compare clientName

instance Eq Room where
    (==) = on (==) roomName

instance Ord Room where
    compare = on compare roomName


data ErrorMsg = ParserError Text
              | InitialConnectionError Text
              | ForbiddenJoinError Text
              deriving Show

instance ToJSON ErrorMsg where
    toJSON (ParserError t) =
        object [ "type" .= ("error" :: Text)
               , "code" .= (1 :: Int)
               , "payload" .= t]
    toJSON (InitialConnectionError t) =
        object [ "type" .= ("error" :: Text)
               , "code" .= (2 :: Int)
               , "payload" .= t]
    toJSON (ForbiddenJoinError t) =
        object [ "type" .= ("error" :: Text)
               , "code" .= (3 :: Int)
               , "payload" .= t]


data IncomingMsg = UserJoin { iRoomName :: !Text }
                 | UserLeave { iRoomName :: !Text }
                 | UserSendText { iRoomName :: !Text
                                , iMsg :: !Text }
            deriving Show


instance FromJSON IncomingMsg where
    parseJSON o@(Object v) = do
        typ <- v .: "type" :: Parser Text
        case typ of
          "join" -> UserJoin <$> v .: "roomname"
          "leave" -> UserLeave <$> v .: "roomname"
          "msg" -> UserSendText <$>
                   v .: "roomname" <*>
                   v .: "payload"
          _ -> typeMismatch "invalid msg type" o
    parseJSON invalid = typeMismatch "Invalid" invalid


data ChanMessage = Broadcast !RoomName !UserName !Text
                 | Tell !UserName !Text
                 | JoinNotice !RoomName !UserName
                 | LeaveNotice !RoomName !UserName
                 | Notice !RoomName !Text


instance ToJSON ChanMessage where
    toJSON (Broadcast r u m) =
        object [ "type" .= ("msg" :: Text)
               , "username" .= u
               , "roomname" .= r
               , "payload" .= m]
    toJSON (JoinNotice r u) =
        object [ "type" .= ("join" :: Text)
               , "username" .= u
               , "roomname" .= r]
    toJSON (LeaveNotice r u) =
        object [ "type" .= ("leave" :: Text)
               , "username" .= u
               , "roomname" .= r]
    toJSON (Tell u m) = undefined
    toJSON (Notice r m) = undefined

instance WS.WebSocketsData (Either ErrorMsg IncomingMsg) where
    fromLazyByteString s =
        case eitherDecode s of
          Left m -> Left . ParserError $ pack m
          Right i -> Right i
    toLazyByteString = undefined

instance WS.WebSocketsData ChanMessage where
    fromLazyByteString = undefined
    toLazyByteString = encode

instance WS.WebSocketsData ErrorMsg where
    fromLazyByteString = undefined
    toLazyByteString = encode
