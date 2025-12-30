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
import Domain.Types (SessionId, UserId)
import GHC.Generics (Generic)

-- | API Request type (JSON用、ドメイン型とは独立)
data CreateSessionRequest = CreateSessionRequest
  { title :: Text,
    purpose :: Text,
    background :: Text,
    hostUserId :: UserId,
    initialQuestions :: Maybe [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CreateSessionResponse = CreateSessionResponse
  { sessionId :: SessionId,
    adminAccessToken :: UUID
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
