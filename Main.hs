module Main where
import System.IO
import Fourmis

main = do
  hSetBuffering stdout NoBuffering
  mapM_ (animeLigne . créeBâton 25)  [0..]
