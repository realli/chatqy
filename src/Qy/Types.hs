module Qy.Types where

import Servant
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Data.Text (Text)

import Qy.Config

type AppM = ReaderT Config Handler

type UserName = Text

toAppM :: Handler a -> AppM a
toAppM = lift

