{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Effect.User where

import Data.Text (Text)
import Domain.Types (UserId)
import Polysemy

data UserEffect m a where
  GetUser :: UserId -> UserEffect m (Maybe Text)

makeSem ''UserEffect
