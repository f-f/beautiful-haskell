module Main where

import qualified Docs
import           Tweet (tweet)

-- | Given a Url to a Doc page, post a new tweet
sendPage :: Docs.Url -> IO()
sendPage url = do
  Just piece <- Docs.moduleDocPiece url
  let msg = Docs.format piece
  tweet msg

main :: IO ()
main = do
  putStrLn "hello world"
