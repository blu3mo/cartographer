{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Web.Types
  ( CreateSessionRequest (..),
    CreateSessionResponse (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Domain.Types (SessionId)
import GHC.Generics (Generic)

-- | API Request type (JSON用、ドメイン型とは独立)
data CreateSessionRequest = CreateSessionRequest
  { title :: Text,
    purpose :: Text,
    background :: Text,
    hostUserId :: UUID
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateSessionResponse = CreateSessionResponse
  { sessionId :: SessionId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
