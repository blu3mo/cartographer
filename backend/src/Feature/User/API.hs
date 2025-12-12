{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Feature.User.API where

import Servant
import Domain.Types (User, NewUser)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] User
