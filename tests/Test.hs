module Main where

import qualified Data.StringMap.Base as SM
import Data.StringMap.LOUDS

import Prelude hiding (lookup)

main :: IO ()
main = do

  let x = SM.fromList [ ("12345", 1 :: Int)
                      , ("12567", 2)
                      , ("23456", 3)
                      , ("3456", 4)
                      ]

  mapM_ (putStrLn . show) (loudsnodes x)

  let LOUDS lds tmn lbl out = louds x

  print out
  print lbl
  print tmn

  print (lookup "3456" (louds x))
  print (lookup "23456" (louds x))
  print (lookup "12345" (louds x))
  print (lookup "12567" (louds x))
