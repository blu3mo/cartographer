module Cartographer.Database where

import Cartographer.Domain
import Data.Text (Text)
import Data.Text qualified as T
import ProjectM36.Base
import ProjectM36.Client

setupSchema :: DatabaseContext -> DatabaseContext
setupSchema ctx =
  -- In a real scenario, we might use registerAtomFunction or similar.
  -- But for Tutorial D export, we generate strings.
  -- Actually, `deriveAtomable` generates `Atomable` instances which allow using Haskell types in M36.
  -- To "export schema", we need to define the Data Types in Tutorial D.
  -- The `project-m36` library has `toMakeDatabaseContextExpr` or similar?
  -- Or strictly speaking, we might need to manually define the mapping if we want to dump Tutorial D text.
  -- But wait, `Atomable` makes it possible to use values. It doesn't automatically create types in DB unless we execute "Data ...".

  -- For this task "Haskell-First Schema", we want to generate:
  -- "data SessionStatus = Draft | Open | Closed"

  -- Since we derived Generic, we can potentially inspect it or just rely on manual mapping for now as per plan?
  -- Plan said: "Setup Schema... execScript"

  execScript ctx "data SessionStatus = Draft | Open | Closed"
  where
    -- And so on.

    -- But to make it robust, we should arguably generate this string from the Type Rep, but that requires more meta-programming.
    -- Given the requirements, I will implement explicit setup for now.

    -- execScript context script = case executeDatabaseContextExpr context (DatabaseContextExpr script) of
    --   _ -> context
    -- Correctly, DatabaseContextExpr logic is more complex to simulate purely without session.
    -- For now, commenting out the invalid usage as we rely on exportTutorialD for this task.
    -- Correctly, DatabaseContextExpr logic is more complex to simulate purely without session.
    -- For now, commenting out the invalid usage as we rely on exportTutorialD for this task.
    execScript :: DatabaseContext -> String -> DatabaseContext
    execScript context _ = context

-- Let's redefine. We want to export Tutorial D.
exportTutorialD :: IO ()
exportTutorialD = do
  putStrLn ":loadexample basic" -- Or start empty
  putStrLn "data SessionStatus = Draft | Open | Closed"
  putStrLn "data QuestionType = FreeText | LikertScale Integer"

  -- Relationships?
  -- "create relvar sessions {id SessionId, status SessionStatus, ...}"
  -- We haven't defined the RelVars in Domain yet, only Value Types.
  -- The User Request: "3. Schema Definition & Export... create relvar events ..."

  -- I need to define the intended schema for 'events'.
  -- "SessionCreated SessionId Text Text" -> Event
  -- Event structure implies: event_id, aggregate_id, event_type, payload?
  -- Or just specific relvars?
  -- M36 is strictly relational.
  -- Event Sourcing in M36: one big 'events' relvar?
  -- "data CartographerEvent = ..."
  -- If we use M36 for event sourcing, we store the ADT value in an atom?
  -- `create relvar events {id UUID, event CartographerEvent, saved_at DateTime}`
  -- Using `Atomable`, `CartographerEvent` can be a column type!

  putStrLn "data CartographerEvent = SessionCreated SessionId Text Text | QuestionAdded SessionId QuestionId Text QuestionType | AnswerPosted SessionId QuestionId Text"
  putStrLn "create relvar events {event_id UUID, event CartographerEvent, timestamp DateTime}"
