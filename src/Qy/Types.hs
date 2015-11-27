module Qy.Types where

import Servant
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Data.Text (Text)

import Qy.Config

type AppM = ReaderT Config (EitherT ServantErr IO)

type UserName = Text

toAppM :: EitherT ServantErr IO a -> AppM a
toAppM = lift

