{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.API where

import Data.Text (Text)
import Domain.Types (UserId)
import Servant.API

type API =
  "health" :> Get '[JSON] Text
    :<|> "users" :> Capture "id" UserId :> Get '[JSON] Text
