{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Infrastructure.DB.Model where

import Data.Text (Text)
import Data.UUID (UUID, fromText)
import Database.Persist (Entity(..))
import Database.Persist.TH
import Domain.Types qualified as Domain

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    uid Text
    name Text
    email Text
    UniqueUid uid
    deriving Show Eq

Blog
    uid Text
    title Text
    content Text
    authorUid Text
    UniqueBlogUid uid
    deriving Show Eq
|]

-- | Convert a DB Entity to a Domain User
toDomainUser :: Entity User -> Domain.User
toDomainUser (Entity _ user) =
    Domain.User
        { Domain.userId = maybe nil id (fromText user.userUid)
        , Domain.userName = user.userName
        , Domain.userEmail = user.userEmail
        }
  where
    nil = error "Invalid UUID in DB"

-- | Convert a DB Entity to a Domain Blog
toDomainBlog :: Entity Blog -> Domain.Blog
toDomainBlog (Entity _ blog) =
    Domain.Blog
        { Domain.blogId = maybe nil id (fromText blog.blogUid)
        , Domain.blogTitle = blog.blogTitle
        , Domain.blogContent = blog.blogContent
        , Domain.blogAuthorId = maybe nil id (fromText blog.blogAuthorUid)
        }
  where
    nil = error "Invalid UUID in DB"
