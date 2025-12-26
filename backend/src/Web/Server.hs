{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Web.Server where

import Data.Text (Text)
import Domain.Types (UserId)
import Effect.User
import Polysemy
import Polysemy.Error
import Servant
import Web.API
import Web.Orphans ()

server :: (Member UserEffect r, Member (Error ServerError) r) => ServerT API (Sem r)
server = healthHandler :<|> userHandler

healthHandler :: Sem r Text
healthHandler = pure "OK"

userHandler :: (Member UserEffect r, Member (Error ServerError) r) => UserId -> Sem r Text
userHandler uid = do
  mUser <- getUser uid
  case mUser of
    Just user -> pure user
    Nothing -> throw err404
