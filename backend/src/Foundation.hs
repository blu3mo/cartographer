{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Foundation where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend, SqlPersistT, runSqlPool)
import Servant (Handler, ServerError)
import UnliftIO (MonadUnliftIO)

-- | The application environment
data Env = Env
  { envDbPool :: Pool SqlBackend
  }

-- | The application monad
newtype AppM a = AppM
  { unAppM :: ReaderT Env (LoggingT Handler) a
  } deriving newtype
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadReader Env
      , MonadError ServerError
      )

-- | Helper to run DB queries
runDB :: SqlPersistT IO a -> AppM a
runDB query = do
  pool <- asks (\env -> env.envDbPool)
  liftIO $ runSqlPool query pool

-- | Runner for the AppM monad (Natural Transformation to Handler)
runApp :: Env -> AppM a -> Handler a
runApp env app = runStdoutLoggingT $ runReaderT app.unAppM env
