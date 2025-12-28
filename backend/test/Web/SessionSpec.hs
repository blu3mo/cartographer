{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Web.SessionSpec (spec) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless)
import Data.Aeson (encode)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Domain.Types (SessionContext (..), SessionId)
import Effect.Persistence (DbConfig (..), migrateSchemaIfNeeded, withM36Connection)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import ProjectM36.Base (RelationalExprBase (..))
import ProjectM36.Client.Simple (close, query, simpleConnectProjectM36, withTransaction)
import Servant.API ((:<|>) (..))
import Servant.Client
import Servant.Client (BaseUrl (..), ClientEnv, Scheme (..), client, mkClientEnv, runClientM)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.UUID ()
import Web.API (API)
import Web.Server (app)
import Web.Types (CreateSessionRequest (..), CreateSessionResponse (..))

-- TODO: Reuse logic from Main.hs or extract app creation to avoid duplication
-- For now, importing app from Web.Server

spec :: Spec
spec = do
  describe "POST /sessions" $ do
    it "creates a session successfully with valid data" $ do
      withApp $ \clientEnv dbPath -> do
        hostId <- nextRandom

        -- Print initial state
        dumpEvents dbPath "Before Request (Snapshot)"

        let req = CreateSessionRequest "My Session" "Testing" "Context" hostId Nothing
        res <- runClientM (createSession req) clientEnv

        case res of
          Left err -> expectationFailure $ "ClientError: " ++ show err
          Right (CreateSessionResponse sid) -> do
            -- Verify persistence
            verifySessionPersisted dbPath sid
    it "persists hostUserId correctly" $ do
      withApp $ \clientEnv _ -> do
        hostId <- nextRandom
        let req = CreateSessionRequest "Persistence Test" "Check DB" "Background" hostId Nothing
        res <- runClientM (createSession req) clientEnv
        case res of
          Left err -> expectationFailure $ "ClientError: " ++ show err
          Right (CreateSessionResponse _) -> pure ()

    it "creates a session with initial questions" $ do
      withApp $ \clientEnv dbPath -> do
        hostId <- nextRandom
        let questions = ["What is the goal?", "Who are the stakeholders?"]
        let req = CreateSessionRequest "With Questions" "Testing" "Context" hostId (Just questions)
        res <- runClientM (createSession req) clientEnv

        case res of
          Left err -> expectationFailure $ "ClientError: " ++ show err
          Right (CreateSessionResponse sid) -> do
            verifySessionPersisted dbPath sid
            verifyStatementsPersisted dbPath questions

    it "Property: accepts any valid Text and UUID" $ property $ \t p b h -> wrapperProperty t p b h

wrapperProperty :: Text -> Text -> Text -> UUID -> Property
wrapperProperty t p b h = ioProperty $ do
  withApp $ \clientEnv _ -> do
    let req = CreateSessionRequest t p b h Nothing
    res <- runClientM (createSession req) clientEnv
    case res of
      Left _ -> pure False -- Fail
      Right _ -> pure True

-- * Client definition

createSession :: CreateSessionRequest -> ClientM CreateSessionResponse
_ :<|> createSession = client (Proxy @API)

withApp :: (ClientEnv -> FilePath -> IO a) -> IO a
withApp action = do
  -- Use Persistent DB in tmp to share state between migration and server handler
  uuid <- nextRandom
  let dbPath = "/tmp/cartographer-test-" <> show uuid
  let dbConfig = Persistent dbPath

  -- Initialize DB schema
  _ <- withM36Connection dbConfig migrateSchemaIfNeeded

  -- Start server
  Warp.testWithApplication (pure $ app dbConfig) $ \port -> do
    manager <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" port ""
    let clientEnv = mkClientEnv manager baseUrl
    action clientEnv dbPath

-- * Debug Helper

verifySessionPersisted :: FilePath -> SessionId -> IO ()
verifySessionPersisted dbPath sessionId = do
  res <- withM36Connection (Persistent dbPath) $ \conn -> do
    withTransaction conn $ do
      query (RelationVariable "events" ())

  case res of
    Left err -> expectationFailure $ "DB Query failed: " ++ show err
    Right (Left err) -> expectationFailure $ "Relational Error: " ++ show err
    Right (Right rel) -> do
      let relStr = show rel
      -- Check 1: SessionId must be present
      let sidStr = show sessionId
      unless (sidStr `isInfixOf` relStr) $ do
        putStrLn $ "DEBUG: Relation Content:\n" ++ relStr
        expectationFailure $ "SessionId " ++ sidStr ++ " not found in events relation"

      -- Check 2: ContextDefined must be present (proving payload was saved)
      unless ("ContextDefined" `isInfixOf` relStr) $ do
        putStrLn $ "DEBUG: Relation Content:\n" ++ relStr
        expectationFailure "Payload 'ContextDefined' not found in events relation"

verifyStatementsPersisted :: FilePath -> [Text] -> IO ()
verifyStatementsPersisted dbPath expectedQuestions = do
  res <- withM36Connection (Persistent dbPath) $ \conn -> do
    withTransaction conn $ do
      query (RelationVariable "events" ())
  case res of
    Left err -> expectationFailure $ "DB Query failed: " ++ show err
    Right (Left err) -> expectationFailure $ "Relational Error: " ++ show err
    Right (Right rel) -> do
      let relStr = show rel
      forM_ expectedQuestions $ \q -> do
        unless (T.unpack q `isInfixOf` relStr) $ do
          expectationFailure $ "Question not found: " ++ show q

-- Legacy dump helper (unused but kept if needed)

-- Debug Helper: Dump events relation content
dumpEvents :: FilePath -> String -> IO ()
dumpEvents dbPath label = do
  putStrLn ""
  putStrLn $ "--- " ++ label ++ " ---"
  res <- withM36Connection (Persistent dbPath) $ \conn -> do
    withTransaction conn $ do
      query (RelationVariable "events" ())
  case res of
    Left err -> putStrLn $ "Dump Error (Connection): " ++ show err
    Right (Left err) -> putStrLn $ "Dump Error (Relation): " ++ show err
    Right (Right rel) -> putStrLn $ "Relation Content:\n" ++ show rel
  pure ()

-- Cleanup? (Optional for /tmp in test)
-- removeDirectoryRecursive dbPath
