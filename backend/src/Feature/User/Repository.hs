{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.User.Repository where

import Control.Monad.IO.Class (liftIO)
import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (selectList, insert)
import Foundation (AppM, runDB)
import Domain.Types (User(..), NewUser(..))
import Infrastructure.DB.Model (toDomainUser)
import qualified Infrastructure.DB.Model as DB

-- | User Repository Interface
class Monad m => UserRepository m where
  findAll :: m [User]
  create :: NewUser -> m User

-- | User Repository Implementation
instance UserRepository AppM where
  findAll = do
    entities <- runDB $ selectList [] []
    return $ map toDomainUser entities

  create newUser = do
    uuid <- liftIO nextRandom
    let dbUser = DB.User
          { DB.userUid = toText uuid
          , DB.userName = newUser.newUserName
          , DB.userEmail = newUser.newUserEmail
          }
    
    _ <- runDB $ insert dbUser
    
    return $ User
      { userId = uuid
      , userName = newUser.newUserName
      , userEmail = newUser.newUserEmail
      }
