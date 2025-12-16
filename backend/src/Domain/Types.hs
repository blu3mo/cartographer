{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Domain.Types
  ( User(..)
  , NewUser(..)
  , Blog(..)
  , NewBlog(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data User = User
  { userId :: UUID
  , userName :: Text
  , userEmail :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NewUser = NewUser
  { newUserName :: Text
  , newUserEmail :: Text
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Blog = Blog
  { blogId :: UUID
  , blogTitle :: Text
  , blogContent :: Text
  , blogAuthorId :: UUID
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data NewBlog = NewBlog
  { newBlogTitle :: Text
  , newBlogContent :: Text
  , newBlogAuthorId :: UUID
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)
