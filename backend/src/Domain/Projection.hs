module Domain.Projection where

import Data.List (sortOn)
import Domain.Types
  ( Event (..),
    FactPayload (..),
    SessionContext,
  )

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
