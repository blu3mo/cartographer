
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (replicateM_, when)
import Data.List (nub)
import Data.Set qualified as Set
import Network.HTTP.Simple
import System.Directory

-- A helper to make the normalization logic available to collectImageUrls
normalizeUrl :: String -> String
normalizeUrl url =
  let trimmed = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') $ url
      normalized = normalizeSlashes trimmed
   in normalized
  where
    normalizeSlashes :: String -> String
    normalizeSlashes [] = []
    normalizeSlashes [x] = [x]
    normalizeSlashes ('h' : 't' : 't' : 'p' : ':' : '/' : '/' : rest) =
      "http://" ++ normalizeSlashes' rest
    normalizeSlashes ('h' : 't' : 't' : 'p' : 's' : ':' : '/' : '/' : rest) =
      "https://" ++ normalizeSlashes' rest
    normalizeSlashes (x : xs) = x : normalizeSlashes xs

    normalizeSlashes' :: String -> String
    normalizeSlashes' [] = []
    normalizeSlashes' [x] = [x]
    normalizeSlashes' ('/' : '/' : xs) = normalizeSlashes' ('/' : xs)
    normalizeSlashes' (x : xs) = x : normalizeSlashes' xs

-- | Remove duplicate URLs after normalizing them (useful for on-page links)
nubUrls :: [String] -> [String]
nubUrls = nub . map normalizeUrl