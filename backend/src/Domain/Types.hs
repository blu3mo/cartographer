module Domain.Types where

import Codec.Winery (Serialise, WineryRecord (..), WineryVariant (..))
import Control.DeepSeq (NFData)
import Data.List (sortOn)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import ProjectM36.Atomable (Atomable)

-- IDエイリアス
type SessionId = UUID

type EventId = UUID

type ParentEventId = Maybe EventId -- グラフのエッジ (NothingならRoot/大質問)

type UserId = UUID

newtype SessionTitle = SessionTitle Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise)
  deriving anyclass (Atomable)

newtype SessionPurpose = SessionPurpose Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (NFData, Serialise)
  deriving anyclass (Atomable)

newtype SessionTopic = SessionTopic Text
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
    topic :: SessionTopic,
    background :: SessionBackground
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData)
  deriving (Serialise) via WineryRecord SessionContext

-- | ファクトのペイロード (Sum型/ADT)
data FactPayload
  = ContextDefined SessionContext
  | QuestionDerived Text ParentEventId
  | Answered Text UserId EventId
  | InsightExtracted Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData)
  deriving (Serialise) via WineryVariant FactPayload

-- | イベント (Record型)
data Event = Event
  { eventId :: EventId,
    sessionId :: SessionId,
    timestamp :: UTCTime,
    payload :: FactPayload
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Atomable, NFData)
  deriving (Serialise) via WineryRecord Event

-- | 射影 (Projection)
-- M36から取得した [Event] を畳み込む
data CurrentState = CurrentState
  { context :: Maybe SessionContext,
    knowledgeGraph :: [FactPayload]
  }
  deriving (Eq, Show)

-- | イベントリストから現在の状態を復元する
project :: [Event] -> CurrentState
project events = foldl applyEvent initialState (sortEvents events)
  where
    initialState = CurrentState Nothing []

    -- イベントを適用して状態を更新する
    applyEvent :: CurrentState -> Event -> CurrentState
    applyEvent state (Event _ _ _ p) = case p of
      ContextDefined ctx -> state {context = Just ctx}
      other -> state {knowledgeGraph = other : state.knowledgeGraph}

    -- イベントを時系列順にソートする
    sortEvents :: [Event] -> [Event]
    sortEvents = sortOn (.timestamp)
