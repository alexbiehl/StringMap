module Main where

import qualified Data.StringMap.Base as SM
import Data.StringMap.LOUDS

import qualified Succinct.Tree.LOUDS as LOUDS

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

  print lds
  print tmn


  let y = LOUDS.root (louds x)
      y' = louds x

  print (lookup "3456" y')
  print (lookup "12345" y')
  print (lookup "12567" y')
  print (lookup "23456" y')
  print (lookup "1234bs" y')
  print (lookup "sadasd" y')
