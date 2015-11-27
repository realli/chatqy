module Qy.Chat.Internal where

import Prelude as P
import Data.Text (Text)
import qualified Data.Text as T
import Control.Concurrent.STM
import Data.HashMap.Strict as M
import qualified Data.Set as Set

import Qy.Chat.Types


getRoom :: RoomName -> RoomMap -> STM Room
getRoom rname rmap = do
    map <- readTVar rmap
    case M.lookup rname map of
      Just room -> return room
      Nothing -> do
          room <- makeEmptyRoom rname
          modifyTVar rmap $ M.insert rname room
          return room

makeEmptyRoom :: RoomName -> STM Room
makeEmptyRoom rname = do
    chan <- newBroadcastTChan
    clients <- newTVar (Set.empty)
    return $ Room rname clients chan

makeNewClient :: UserName -> STM Client
makeNewClient uname = do
    chan <- newTChan
    rooms <- newTVar (Set.empty)
    roomChans <- newTVar []
    return $ Client uname rooms chan roomChans

addClientToRoom :: Client -> Room -> STM ()
addClientToRoom client@Client{..} room@Room{..} = do
    roomSet <- readTVar clientRooms
    newRoomChan <- dupTChan roomChan
    let newRoomSet = Set.insert (room {roomChan = newRoomChan}) roomSet
    writeTVar clientRooms newRoomSet
    -- cache all room's Tchan as a flat list
    writeTVar clientRoomChans $ P.map getRoomChan $ Set.toAscList newRoomSet
    modifyTVar roomClients $ Set.insert client

kickClientOutOfRoom :: Client -> Room -> STM ()
kickClientOutOfRoom client@Client{..} room@Room{..} = do
    roomSet <- readTVar clientRooms
    let newRoomSet = Set.delete room roomSet
    writeTVar clientRooms newRoomSet
    -- cache all room's Tchan as a flat list
    writeTVar clientRoomChans $ P.map getRoomChan $ Set.toAscList newRoomSet
    modifyTVar roomClients $ Set.delete client

getAvaliableMessage :: Client -> STM ChanMessage
getAvaliableMessage Client{..} = do
    chans <- readTVar clientRoomChans
    foldl orElse retry $ P.map readTChan chans

clientInRoom :: Client -> Room -> STM Bool
clientInRoom Client{..} room = do
    roomSet <- readTVar clientRooms
    return $ Set.member room roomSet

allClientsInRoom :: Room -> STM [Client]
allClientsInRoom Room{..} = do
    clientSet <- readTVar roomClients
    return $ Set.toAscList clientSet

allRoomClientIn :: Client -> STM [Room]
allRoomClientIn Client{..} = do
    roomSet <- readTVar clientRooms
    return $ Set.toAscList roomSet


