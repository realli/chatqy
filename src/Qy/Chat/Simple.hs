module Qy.Chat.Simple where

import Data.Monoid (mappend)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (forever)
import Data.Foldable (forM_)
import Control.Monad.Trans.Maybe
import Control.Monad.Reader hiding (forM_)
import Control.Exception (finally)

import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.Async (race_)

import Web.JWT (iss, stringOrURIToText)

import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets (websocketsOr)
import Servant (fromText)

import Qy.User (Token(..), getClaimSetFromToken, checkExpValid)
import Qy.Chat.Internal
import Qy.Chat.Types
import Qy.Model (checkUserInRoom)
import Qy.Config


type ChatApp = ReaderT Config IO

raceChatApp :: Config -> ChatApp a -> ChatApp b -> IO ()
raceChatApp cfg left right = (runReaderT left cfg)
    `race_` (runReaderT right cfg)

receive :: WS.Connection -> ChatApp (Either ErrorMsg IncomingMsg)
receive = liftIO . WS.receiveData

send :: (WS.WebSocketsData a) => WS.Connection -> a -> ChatApp ()
send conn = liftIO . WS.sendTextData conn


hoistMaybeT :: (Monad m) => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . return

extractUserInfo :: Text -> MaybeT IO Text
extractUserInfo t = do
    let token = Token t
    claim <- hoistMaybeT $ getClaimSetFromToken token
    isValid <- liftIO $ checkExpValid claim
    if isValid
    then do
        uid <- hoistMaybeT $ iss claim
        return $ stringOrURIToText uid
    else hoistMaybeT Nothing


application :: Config -> WS.PendingConnection -> IO ()
application cfg pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    msg <- WS.receiveData conn
    result <- runMaybeT $ extractUserInfo msg
    case result of
      Nothing -> WS.sendTextData conn $
        InitialConnectionError "Auth Failed, User not logged in"
      Just uname -> do
          client <- liftIO . atomically $ makeNewClient uname
          raceChatApp cfg
              (receiveLoop client conn)
              (sendLoop client conn) 
            `finally` disconnect client

disconnect :: Client -> IO ()
disconnect client@Client{..} = atomically $ do
    roomSet <- readTVar clientRooms
    forM_ roomSet (kickClientOutOfRoom client) 
    forM_ roomSet (\room -> writeTChan (roomChan room) $ LeaveNotice (roomName room) clientName)

appWithSocket cfg = websocketsOr WS.defaultConnectionOptions (application cfg)

receiveLoop :: Client -> WS.Connection -> ChatApp ()
receiveLoop client@Client{..} conn = do
    rmap <- asks getRoomMap
    forever $ do
        receivedMsg <- receive conn
        case receivedMsg of
          Left err -> send conn err
          Right (UserJoin r) -> do
              userInRoom <- checkUserInRoom clientName r
              if userInRoom
              then liftIO . atomically $ do
                  room <- getRoom r rmap
                  isIn <- clientInRoom client room
                  guard (not isIn)
                  addClientToRoom client room
                  writeTChan (roomChan room) $ JoinNotice r clientName
              else send conn $ ForbiddenJoinError "Not a Member"
          Right (UserLeave r) -> liftIO . atomically $ do
              room <- getRoom r rmap
              isIn <- clientInRoom client room
              guard isIn
              kickClientOutOfRoom client room
              writeTChan (roomChan room) $ LeaveNotice r clientName
          Right (UserSendText r i) -> liftIO . atomically $ do
              room <- getRoom r rmap
              isIn <- clientInRoom client room
              guard isIn
              writeTChan (roomChan room) $ Broadcast r clientName i

sendLoop :: Client -> WS.Connection -> ChatApp ()
sendLoop client@Client{..} conn = forever $ do
    chanMsg <- liftIO . atomically $ do
        getAvaliableMessage client
    send conn chanMsg






