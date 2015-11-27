module Qy.Error where

import Servant
import Control.Monad.Trans.Either
import Control.Monad.Reader

import Qy.Types (AppM)

class ToServantErr a where
    toServantErr :: a -> ServantErr

instance ToServantErr ServantErr where
    toServantErr = id

raiseHTTPError :: (ToServantErr e) => e -> AppM a
raiseHTTPError = lift . left . toServantErr
