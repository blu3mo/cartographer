{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Implementation.User where

import Domain.Types (UserId)
import Effect.User
import Polysemy

runUserPure :: Sem (UserEffect ': r) a -> Sem r a
runUserPure = interpret $ \case
  GetUser _ -> pure (Just "Alice")
