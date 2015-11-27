module Qy.Config (
    Config
    , getPool
    , getEnv
    , getTokenMap
    , getRoomMap
    , defaultConfig
    , setLogger
    , makePool
    , makeTokenMap
    , makeRoomMap
    )
where

import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai (Middleware)
import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import Data.HashMap.Strict as M
import Control.Monad
import qualified Control.Applicative as A
import Data.Aeson

import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, ConnectionString)

import Qy.Chat.Types (RoomMap, emptyRoomMap)
import Qy.RefreshToken.HashMap (RTokenMap, newRTokenMap)

data Environment = Development | Test | Production
    deriving (Eq, Show, Read)

instance FromJSON Environment where
    parseJSON (String s)
        | s == "production" = return Production
        | s == "development" = return Development
        | s == "test" = return Test

    parseJSON _ = A.empty

data Config = Config
    { getPool :: ConnectionPool
    , getEnv:: Environment
    , getTokenMap :: RTokenMap
    , getRoomMap :: RoomMap
    }

defaultConfig :: Config
defaultConfig = Config { getPool = undefined
                       , getEnv = Development
                       , getTokenMap = undefined
                       , getRoomMap = undefined
                       }

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

makePool :: Environment -> ConnectionString -> Int -> IO ConnectionPool
makePool Test s poolNum = runNoLoggingT $ createPostgresqlPool s poolNum
makePool e s poolNum = runStdoutLoggingT $ createPostgresqlPool s poolNum

makeTokenMap :: IO RTokenMap
makeTokenMap = newRTokenMap

makeRoomMap :: IO RoomMap
makeRoomMap = emptyRoomMap
