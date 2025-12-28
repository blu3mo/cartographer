module Domain.Types where

import Codec.Winery (Serialise, WineryRecord (..), WineryVariant (..))
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import ProjectM36.Atomable (Atomable)
import ProjectM36.Tupleable (Tupleable)

-- IDエイリアス
type SessionId = UUID

type UserId = UUID

type EventId = UUID

type StatementId = UUID

newtype StatementContent = StatementContent Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise, FromJSON, ToJSON)
  deriving anyclass (Atomable)

data Statement = Statement
  { id :: StatementId,
    content :: StatementContent
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData, FromJSON, ToJSON)
  deriving (Serialise) via WineryRecord Statement

newtype SessionTitle = SessionTitle Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise, FromJSON, ToJSON)
  deriving anyclass (Atomable)

newtype SessionPurpose = SessionPurpose Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise, FromJSON, ToJSON)
  deriving anyclass (Atomable)

newtype SessionBackground = SessionBackground Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise, FromJSON, ToJSON)
  deriving anyclass (Atomable)

-- | セッションのコンテキスト情報 (Record型)
data SessionContext = SessionContext
  { title :: SessionTitle,
    purpose :: SessionPurpose,
    background :: SessionBackground,
    hostUserId :: UserId
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData, FromJSON, ToJSON)
  deriving (Serialise) via WineryRecord SessionContext

-- | ファクトのペイロード (Sum型/ADT)
data FactPayload
  = ContextDefined SessionContext
  | StatementAdded Statement
  | ReportGenerated Text EventId
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData)
  deriving (Serialise) via WineryVariant FactPayload

data Event = Event
  { eventId :: EventId,
    sessionId :: SessionId,
    timestamp :: UTCTime,
    payload :: FactPayload
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData, Tupleable)
  deriving (Serialise) via WineryRecord Event
