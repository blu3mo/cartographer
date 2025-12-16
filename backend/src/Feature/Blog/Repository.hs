{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Feature.Blog.Repository where

import Control.Monad.IO.Class (liftIO)
import Data.UUID (UUID, toText)
import Data.UUID.V4 (nextRandom)
import Database.Persist (selectList, insert, (==.))
import Foundation (AppM, runDB)
import Domain.Types (Blog(..), NewBlog(..))
import Infrastructure.DB.Model (toDomainBlog)
import qualified Infrastructure.DB.Model as DB

-- | Blog Repository Interface
class Monad m => BlogRepository m where
  findBlogsByAuthor :: UUID -> m [Blog]
  createBlog :: NewBlog -> m Blog

-- | Blog Repository Implementation
instance BlogRepository AppM where
  findBlogsByAuthor authorId = do
    let authorUidText = toText authorId
    entities <- runDB $ selectList [DB.BlogAuthorUid ==. authorUidText] []
    return $ map toDomainBlog entities

  createBlog newBlog = do
    uuid <- liftIO nextRandom
    let dbBlog = DB.Blog
          { DB.blogUid = toText uuid
          , DB.blogTitle = newBlog.newBlogTitle
          , DB.blogContent = newBlog.newBlogContent
          , DB.blogAuthorUid = toText newBlog.newBlogAuthorId
          }
    
    _ <- runDB $ insert dbBlog
    
    return $ Blog
      { blogId = uuid
      , blogTitle = newBlog.newBlogTitle
      , blogContent = newBlog.newBlogContent
      , blogAuthorId = newBlog.newBlogAuthorId
      }
