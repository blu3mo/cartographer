module Domain.Types where

import Codec.Winery (Serialise, WineryRecord (..), WineryVariant (..))
import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import ProjectM36.Atomable (Atomable)
import ProjectM36.Tupleable (Tupleable)

-- IDエイリアス
type SessionId = UUID

type EventId = UUID

newtype SessionTitle = SessionTitle Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise)
  deriving anyclass (Atomable)

newtype SessionPurpose = SessionPurpose Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise)
  deriving anyclass (Atomable)

newtype SessionBackground = SessionBackground Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise)
  deriving anyclass (Atomable)

-- | セッションのコンテキスト情報 (Record型)
data SessionContext = SessionContext
  { title :: SessionTitle,
    purpose :: SessionPurpose,
    background :: SessionBackground
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData)
  deriving (Serialise) via WineryRecord SessionContext

-- | ファクトのペイロード (Sum型/ADT)
data FactPayload
  = ContextDefined SessionContext
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
