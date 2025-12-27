{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Web.API.SessionSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Effect.Persistence (DbConfig (..), migrateSchema, runPersistence, withM36Connection)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (testWithApplication)
import Polysemy (Embed, Member, Sem, embed, interpret, runM)
import Polysemy.Error (runError)
import Servant
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import System.Directory (getTemporaryDirectory, removeDirectoryRecursive)
import Test.Hspec
import Web.API (API)
import Web.Server (server)
import Web.Types (CreateSessionRequest (..), CreateSessionResponse (..))

-- | App Stack
runApp :: DbConfig -> Sem '[Embed IO, Error ServerError] a -> Handler a
runApp dbConfig sem = do
  res <- liftIO $ runM $ runError $ runPersistence dbConfig sem
  case res of
    Left err -> throwError err
    Right a -> pure a

-- | Servant Client
api :: Proxy API
api = Proxy

postSession :: CreateSessionRequest -> ClientM CreateSessionResponse
_ :<|> postSession = client api

spec :: Spec
spec = describe "POST /sessions" $ do
  it "creates a session successfully" $ do
    -- Use Persistent DB in temp dir to ensure schema survives across connections
    tempDir <- getTemporaryDirectory
    uid <- nextRandom
    let dbPath = tempDir ++ "/cartographer-test-" ++ show uid
        dbConfig = Persistent dbPath

    -- Initialize Schema
    initRes <- withM36Connection dbConfig $ \conn -> do
      migrateSchema conn
    case initRes of
      Left err -> expectationFailure $ "Migration failed: " ++ show err
      Right (Left err) -> expectationFailure $ "Migration failed: " ++ show err
      Right (Right ()) -> pure ()

    let app = serve api (hoistServer api (runApp dbConfig) server)

    testWithApplication (pure app) $ \port -> do
      manager <- newManager defaultManagerSettings
      let baseUrl = BaseUrl Http "localhost" port ""
          env = mkClientEnv manager baseUrl
          req =
            CreateSessionRequest
              { title = "Test Title",
                purpose = "Test Purpose",
                background = "Test Background"
              }

      res <- runClientM (postSession req) env
      case res of
        Left err -> expectationFailure $ "Client error: " ++ show err
        Right (CreateSessionResponse _sid) -> do
          -- Verify we got a UUID back (simple check)
          pure ()
