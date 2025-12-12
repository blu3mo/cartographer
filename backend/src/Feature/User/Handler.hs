{-# LANGUAGE FlexibleContexts #-}

module Feature.User.Handler where

import Servant
import Foundation (AppM)
import Domain.Types (User, NewUser)
import Feature.User.API (UserAPI)
import Feature.User.Repository (UserRepository(..))

server :: ServerT UserAPI AppM
server = getUsers :<|> createUser

getUsers :: (UserRepository m) => m [User]
getUsers = findAll

createUser :: (UserRepository m) => NewUser -> m User
createUser = create
