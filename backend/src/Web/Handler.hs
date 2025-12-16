{-# LANGUAGE FlexibleContexts #-}

module Web.Handler where

import Servant
import Foundation (AppM)
import Web.API (API)
import Feature.User.API (UserAPI)
import qualified Feature.User.Handler as User
import Feature.Blog.API (BlogAPI)
import qualified Feature.Blog.Handler as Blog

-- | Server implementation
server :: ServerT API AppM
server = User.server :<|> Blog.server
