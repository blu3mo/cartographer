{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Feature.Blog.API where

import Servant
import Data.UUID (UUID)
import Domain.Types (Blog, NewBlog)

type BlogAPI = "users" :> Capture "userId" UUID :> "blogs" :> Get '[JSON] [Blog]
          :<|> "blogs" :> ReqBody '[JSON] NewBlog :> Post '[JSON] Blog
