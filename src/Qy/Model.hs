{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Qy.Model where

import Prelude as P
import Control.Monad.IO.Class  (liftIO)
import Database.Esqueleto
import qualified Database.Persist as P
-- import Database.Persist.Postgresql
import Database.Persist.TH
import Control.Monad.Reader
import Data.Text
import Data.ByteString (ByteString)

import Qy.Config
import Qy.Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Text
    password ByteString
    email Text
    UniqueUser name
    deriving Show
Room
    name Text
    description Text Maybe
    createdBy UserId
    UniqueRoom name
    deriving Show
UserRoom
    userId UserId
    roomId RoomId
    UniqueUserRoom userId roomId
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

checkUserInRoom uname rname = do
    pool <- asks getPool
    rooms <- runDb $ select $
      from $ \(u, re, r) -> do
        where_ (u^.UserId ==. re^.UserRoomUserId
               &&. re^.UserRoomRoomId ==. r^.RoomId
               &&. u^.UserName ==. val uname
               &&. r^.RoomName ==. val rname)
        return r
    return (not $ P.null rooms)
