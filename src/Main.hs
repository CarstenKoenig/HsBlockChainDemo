module Main where

import Chain
import System.IO (hSetBuffering, BufferMode(..), stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

  putStrLn "start..."
  let ch = addBlock "World" $ addBlock "Hello " $ empty 5
  print ch
  putStrLn "round 2"
  print ch
  putStrLn "round 3"
  print $ addBlock "!!" ch
