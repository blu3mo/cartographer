{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.API where

import Data.Text (Text)
import Domain.Types (UserId)
import Servant.API
import Web.Types (CreateSessionRequest, CreateSessionResponse)

type API =
  "health" :> Get '[JSON] Text
    :<|> "sessions" :> ReqBody '[JSON] CreateSessionRequest :> Post '[JSON] CreateSessionResponse
