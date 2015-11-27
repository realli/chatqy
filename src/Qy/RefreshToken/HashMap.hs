module Qy.RefreshToken.HashMap (
    RTokenMap
    , newRTokenMap
    , addToken'
    , existsToken'
    , deleteToken'
    ) where

import Data.HashMap.Strict as M
import Data.Text
import Control.Concurrent.STM
import Control.Monad

type RTokenMap = TVar (HashMap Text Text)

newRTokenMap :: IO RTokenMap
newRTokenMap = atomically $ newTVar M.empty

addToken' :: RTokenMap -> Text -> Text -> IO ()
addToken' rm uid token = atomically $ do
    map <- readTVar rm
    writeTVar rm $ insert uid token map

existsToken' :: RTokenMap -> Text -> Text -> IO Bool
existsToken' rm uid token = do
    map <- readTVarIO rm
    return $ maybe False (== token) (M.lookup uid map)

deleteToken' :: RTokenMap -> Text -> IO ()
deleteToken' rm uid = atomically $ do
    map <- readTVar rm
    writeTVar rm $ delete uid map
