{-# LANGUAGE FlexibleContexts #-}

module Feature.Blog.Handler where

import Servant
import Data.UUID (UUID)
import Foundation (AppM)
import Domain.Types (Blog, NewBlog)
import Feature.Blog.API (BlogAPI)
import Feature.Blog.Repository (BlogRepository(..))

server :: ServerT BlogAPI AppM
server = getBlogs :<|> create

getBlogs :: (BlogRepository m) => UUID -> m [Blog]
getBlogs = findBlogsByAuthor

create :: (BlogRepository m) => NewBlog -> m Blog
create = createBlog
