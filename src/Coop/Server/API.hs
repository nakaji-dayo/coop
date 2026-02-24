module Coop.Server.API
  ( API
  , api
  , HealthAPI
  , healthApi
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Aeson (Value)
import Servant

type API =
       "health" :> Get '[JSON] Value
  :<|> "slack" :> "events"
       :> Header "X-Slack-Signature" Text
       :> Header "X-Slack-Request-Timestamp" Text
       :> ReqBody '[OctetStream] ByteString
       :> Post '[JSON] Value

api :: Proxy API
api = Proxy

type HealthAPI = "health" :> Get '[JSON] Value

healthApi :: Proxy HealthAPI
healthApi = Proxy
