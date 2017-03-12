module Qy.App where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Servant
import Network.Wai.Handler.Warp (run)
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Data.Text
import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Postgresql (withPostgresqlPool)

import Web.JWT hiding (JSON)

import Qy.User (UserAPI, loginServer, Token,
            ReturnToken(..), checkToken)

import Qy.Error (raiseHTTPError)

import Qy.Model
import Qy.Types
import Qy.Config (Config)
import Qy.Page (egServer, Eg)
import Qy.Room (RoomAPI, roomServer)

type API = "api" :> QyAPI
      :<|> "static" :> Raw
      :<|> Eg

type QyAPI = UserAPI :<|> LoginRequiredAPI

type LoginRequiredAPI = Header "Authorization" Token :>
    RoomAPI


server :: ServerT QyAPI AppM
server = loginServer 
    :<|> loginRequiredServer 

loginRequiredServer :: ServerT LoginRequiredAPI AppM
loginRequiredServer t = roomServer t

readerServer :: Config -> Server API
readerServer cfg = enter (readerToEither cfg) server
                   :<|> serveDirectory "build"
                   :<|> egServer

readerToEither :: Config -> AppM :~> Handler
readerToEither cfg = Nat $ \x -> runReaderT x cfg

api :: Proxy API
api = Proxy

app :: Config -> Application
app cfg = serve api (readerServer cfg)
