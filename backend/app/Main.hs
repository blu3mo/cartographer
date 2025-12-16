{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Cartographer.Database (initDatabase)
import Configuration.Dotenv (defaultConfig, loadFile, onMissingFile)
import Lib (startApp)

main :: IO ()
main = do
  onMissingFile (loadFile defaultConfig) (return ())
  putStrLn "Initializing Database..."

  -- Initialize Database (M36)
  (sessionId, conn) <- initDatabase

  putStrLn "Starting Cartographer Backend..."
  startApp sessionId conn
