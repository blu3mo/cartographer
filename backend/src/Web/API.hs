{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Web.API 
  ( API
  ) where

import Feature.User.API (UserAPI)
import Feature.Blog.API (BlogAPI)
import Servant.API (type (:<|>))

type API = UserAPI :<|> BlogAPI
