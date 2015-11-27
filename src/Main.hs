module Main where

import Prelude hiding (lookup)
import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)
import Database.Persist.Postgresql (runSqlPool)
import Data.Yaml.Config (load, subconfig, lookupDefault, lookup)
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative ((<$>))

import Qy.Config
import Qy.App    (app)
import Qy.Model (doMigrations)
import Qy.Types

import Qy.Chat.Simple (appWithSocket)

main :: IO ()
main = do
    config <- load "./chatqy.yaml"

    qyConfig <- subconfig "chatqy" config

    let port = lookupDefault "port" 8081 qyConfig
        poolNum = lookupDefault "poolNum" 1 qyConfig
    connStr <- encodeUtf8 <$> lookup "connStr" qyConfig
    env <- lookup "env" qyConfig

    pool <- makePool env connStr poolNum
    rmap <- makeTokenMap
    tmap <- makeRoomMap

    let cfg = defaultConfig { getPool = pool
                            , getEnv = env
                            , getTokenMap = rmap
                            , getRoomMap = tmap}
        logger = setLogger env
    runSqlPool doMigrations pool
    run port . logger . appWithSocket cfg $ app cfg
