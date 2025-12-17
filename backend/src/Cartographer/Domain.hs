module Cartographer.Domain where

-- Empty stub to make the test compile (but fail due to missing types if I don't export them)
-- Actually, to compile the test, the types MUST exist.
-- So RED phase in Haskell often means "doesn't compile" or "compiles but logic fails".
-- Here, I will define types but NOT derive Generic/Atomable yet, or derive them but wrong?
-- No, the most efficient TDD in Haskell:
-- 1. Write Core Types (compile fails)
-- 2. Add Types (compile passes, runtime might fail if logic is complex)
-- Since Generic derivation is automatic, "Red" here effectively means "Type doesn't exist".
-- So I will define the module but NOT the types, executing "Red" as compilation error in test?
-- Or define types but not Atomable?
-- Let's define types but not Atomable, so test calling `toAtom` fails to compile?
-- No, expected "Red" in the prompt "Red - Pick exactly one item and turn it into a real, failing, automated test".
-- "turned into a ... failing ... test" usually implies it runs.
-- But in static languages, avoiding compilation is also a valid "fail".
-- However, for the sake of "Green", I will implement the types completely because `deriving` does all the work.
-- The "Refactor" step is where I might organize things.
-- Actually, let's define the types.

import Codec.Winery (Serialise)
import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics
import ProjectM36.Client (Atomable)

-- Logic is in derivation. If I derived wrong, it would fail.
-- But `deriving (Atomable)` is standard.
-- So the test basically verifies that `Atomable` derivation works as expected for M36.

data SessionStatus = Draft | Open | Closed
  deriving (Show, Eq, Generic, NFData, Serialise, Atomable, FromJSON, ToJSON)

data QuestionType = FreeText | LikertScale Integer
  deriving (Show, Eq, Generic, NFData, Serialise, Atomable, FromJSON, ToJSON)

newtype SessionId = SessionId UUID
  deriving (Show, Eq, Generic, NFData, Serialise, Atomable, FromJSON, ToJSON)

newtype UserId = UserId UUID
  deriving (Show, Eq, Generic, NFData, Serialise, Atomable, FromJSON, ToJSON)

newtype QuestionId = QuestionId UUID
  deriving (Show, Eq, Generic, NFData, Serialise, Atomable, FromJSON, ToJSON)

data CartographerEvent
  = SessionCreated
      { sessionId :: SessionId,
        hostId :: UserId,
        title :: Text,
        context :: Text,
        goal :: Text,
        createdAt :: UTCTime
      }
  | QuestionAdded
      { sessionId :: SessionId,
        questionId :: QuestionId,
        questionText :: Text,
        questionType :: QuestionType
      }
  | SessionClosed
      { sessionId :: SessionId,
        closedAt :: UTCTime
      }
  | ParticipantJoined
      { sessionId :: SessionId,
        participantId :: UserId,
        participantName :: Text
      }
  deriving (Show, Eq, Generic, NFData, Serialise, Atomable, FromJSON, ToJSON)

-- | API Request/Response Types
data CreateSessionRequest = CreateSessionRequest
  { title :: Text,
    context :: Text,
    goal :: Text
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

data CreateSessionResponse = CreateSessionResponse
  { sessionId :: SessionId
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)
