module Qy.RefreshToken (
    addToken
    , existsToken
    , deleteToken
    ) where

import Data.Text
import Control.Monad
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)

import Qy.RefreshToken.HashMap
import Qy.Config
import Qy.Types

addToken uid token = do
    rmap <- asks getTokenMap
    liftIO $ addToken' rmap uid token

existsToken uid token = do
    rmap <- asks getTokenMap
    liftIO $ existsToken' rmap uid token

deleteToken uid = do
    rmap <- asks getTokenMap
    liftIO $ deleteToken' rmap uid

