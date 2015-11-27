module Qy.Room where

import Data.Aeson
import GHC.Generics hiding (from)
import Servant
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Control.Applicative ((<$>), (<*>), empty)
import qualified Control.Applicative as A
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)

import Database.Esqueleto
import qualified Database.Persist as P

import Qy.Types
import qualified Qy.Model as M

import Qy.Error (raiseHTTPError)
import Qy.Error.Room ( roomNotExists
                     , roomAlreadyExists
                     , roomNameNotMatchBody)
import Qy.Error.Auth (userNotExists)
import Qy.User (checkToken, Token)



type RoomAPI = "room" :> Capture "roomname" Text :> "join" :> Post '[JSON] RoomInfo -- join in
          :<|> "room" :> Capture "roomname" Text :> "info" :> Get '[JSON] RoomInfo -- get room info
          :<|> "room" :> Capture "roomname" Text :> ReqBody '[JSON] RoomReq :> Post '[JSON] RoomInfo -- update room info
          :<|> "room" :> Capture "roomname" Text :> Delete '[JSON] RoomInfo -- delete room
          :<|> "rooms" :> ReqBody '[JSON] RoomReq :> Post '[JSON] RoomInfo -- create new room
          :<|> "rooms" :> QueryFlag "all" :> Get  '[JSON] [RoomInfo] -- get all room info user joinded in

data RoomInfo = RoomInfo { name :: Text
                         , description :: Maybe Text
                         } deriving (Show, Generic)

instance ToJSON RoomInfo

data RoomReq = RoomReq Text (Maybe Text)

instance FromJSON RoomReq where
    parseJSON (Object v) = RoomReq 
              <$> v .: "name"
              <*> v .: "description"

roomServer :: Maybe Token -> ServerT RoomAPI AppM
roomServer mt = joinRoom mt
    :<|> getRoomInfo mt
    :<|> updateRoom mt
    :<|> leaveRoom mt
    :<|> createRoom mt
    :<|> listRooms mt

getRoomInfo :: Maybe Token -> Text -> AppM RoomInfo
getRoomInfo mt rname = do
    uname <- checkToken mt
    maybeRoom <- M.runDb $ getBy $ M.UniqueRoom rname
    case maybeRoom of
      Nothing -> raiseHTTPError roomNotExists
      Just (Entity _ room) -> return $ RoomInfo rname (M.roomDescription room)

joinRoom :: Maybe Token -> Text -> AppM RoomInfo
joinRoom mt rname = do
    uname <- checkToken mt
    maybeUser <- M.runDb $ getBy $ M.UniqueUser uname
    maybeRoom <- M.runDb $ getBy $ M.UniqueRoom rname
    case (maybeUser, maybeRoom) of
      (Just (Entity uid _) , Just (Entity rid room))-> do
          M.runDb $ insertBy $ M.UserRoom uid rid
          return $ RoomInfo rname (M.roomDescription room)
      (Nothing, _) -> raiseHTTPError userNotExists
      (_, Nothing) -> raiseHTTPError roomNotExists

leaveRoom :: Maybe Token -> Text -> AppM RoomInfo
leaveRoom mt rname = do
    uname <- checkToken mt
    maybeUser <- M.runDb $ getBy $ M.UniqueUser uname
    maybeRoom <- M.runDb $ getBy $ M.UniqueRoom rname
    case (maybeUser, maybeRoom) of
      (Just (Entity uid _) , Just (Entity rid room))-> do
          M.runDb $ deleteBy $ M.UniqueUserRoom uid rid
          return $ RoomInfo rname (M.roomDescription room)
      (Nothing, _) -> raiseHTTPError userNotExists
      (_, Nothing) -> raiseHTTPError roomNotExists

createRoom :: Maybe Token -> RoomReq -> AppM RoomInfo
createRoom mt (RoomReq name desc) = do
    uname <- checkToken mt
    maybeUser <- M.runDb $ getBy $ M.UniqueUser uname
    case maybeUser of
      Just (Entity uid _) -> do
          maybeRoom <- M.runDb $ insertBy $ M.Room name desc uid
          case maybeRoom of
            Left _ -> raiseHTTPError roomAlreadyExists
            Right _ -> return $ RoomInfo name desc
      Nothing -> raiseHTTPError userNotExists

updateRoom :: Maybe Token -> Text -> RoomReq -> AppM RoomInfo
updateRoom mt rname (RoomReq name desc) = do
    uname <- checkToken mt
    if rname /= name
    then raiseHTTPError roomNameNotMatchBody
    else do
        M.runDb $ P.updateWhere 
                [M.RoomName P.==. rname]
                [M.RoomDescription P.=. desc]
        return $ RoomInfo rname desc

listRooms :: Maybe Token -> Bool -> AppM [RoomInfo]
listRooms mt listAll = do
    uname <- checkToken mt
    rooms <- M.runDb $ select $
      from $ \(u, re, r) -> do
          where_ (if listAll
                 then (u^. M.UserId ==. re^. M.UserRoomUserId
                      &&. re^. M.UserRoomRoomId ==. r^. M.RoomId)
                 else (u^. M.UserId ==. re^. M.UserRoomUserId
                      &&. re^. M.UserRoomRoomId ==. r^. M.RoomId
                      &&. u ^. M.UserName ==. val uname))
          return r
    return $ map roomsToInfo rooms
  where roomsToInfo (Entity _ room) = RoomInfo (M.roomName room) (M.roomDescription room)

