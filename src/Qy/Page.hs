module Qy.Page where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Servant.HTML.Blaze
import Servant


page :: Html
page = docTypeHtml $ do
    H.head $ do
        H.title  "ChatQy"
        H.meta ! charset "UTF-8"
        H.link ! rel "stylesheet" ! href "/static/app.css"
    H.body $ do
        H.script ! src "/static/bundle.js" $ ""

type Eg = Get '[HTML] Html
     :<|> "login" :> Get '[HTML] Html
     :<|> "signup" :> Get '[HTML] Html


egServer :: Server Eg
egServer = return page
      :<|> return page
      :<|> return page
