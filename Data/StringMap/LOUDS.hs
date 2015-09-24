{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.StringMap.LOUDS where

import           Data.StringMap.Base         as SM hiding (lookup, null, (!))

import           Succinct.Dictionary.Builder
import           Succinct.Dictionary.Class
import           Succinct.Dictionary.Poppy

import Data.Foldable
import           Data.Bits
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Primitive       as P

import           Debug.Trace

import           Prelude                     hiding (lookup)

loudsnodes :: StringMap a -> [StringMap a]
loudsnodes sm = go 0 where
  go n = case level n (SM.norm sm) [] of
    [] -> []
    ys -> ys ++ go (n + 1)
{-# INLINE loudsnodes #-}

level :: Int -> StringMap a -> [StringMap a] -> [StringMap a]
level 0 sm xs = (norm sm:smsiblings sm) ++ xs
level n sm xs = Prelude.foldr (level (n - 1) . SM.norm) xs (smchildren sm)

smsiblings :: StringMap a -> [StringMap a]
smsiblings =  go . SM.norm
  where
    go Empty = []
    go (Val _ _) = []
    go (Branch _ child next) = SM.norm next:go (norm next)
{-# INLINE smsiblings #-}

smchildren :: StringMap a -> [StringMap a]
smchildren = go . SM.norm
  where
    go Empty = []
    go (Branch sym child next) = child : go (SM.norm next)
    go (Val value child) = [SM.norm child]
{-# INLINE smchildren #-}

data BuildLOUDS louds terminal label output =
  BuildLOUDS louds terminal label output

data LOUDS a =
  LOUDS !Poppy !Poppy !(P.Vector Char) !(V.Vector a)
  deriving (Show)

instance Buildable (StringMap a) (LOUDS a) where
  builder = Builder $ case builder :: Builder  Bool Poppy of
    Builder (Building kl hl zl) -> case builder :: Builder Bool Poppy of -- louds
      Builder (Building kt ht zt) -> case builder :: Builder Char (P.Vector Char) of -- terminal
        Builder (Building kc hc zc) -> case builder :: Builder a (V.Vector a) of -- labels/characters
          Builder (Building ko ho zo) -> Building stop step start where -- output

            start = BuildLOUDS
                    <$> (zl >>= \l -> hl l True >>= \l -> hl l False)
                    <*> (zt >>= \t -> ht t False >>= \t -> ht t False)
                    <*> (zc >>= \c -> hc c '\0' >>= \c -> hc c '\0')
                    <*> zo

            step (BuildLOUDS louds terminal label output) Empty  = do
              louds' <- hl louds False
              terminal' <- ht terminal False
              label' <- hc label '\0'
              return (BuildLOUDS louds' terminal' label' output)

            step (BuildLOUDS louds terminal label output) (Branch sym _ _)  = do
              louds' <- hl louds True
              terminal' <- ht terminal False
              label' <- hc label sym
              return (BuildLOUDS louds' terminal label' output)

            step (BuildLOUDS louds terminal label output) (Val value _)  = do
              louds' <- hl louds False
              terminal' <- ht terminal True
              label' <- hc label '\0'
              output' <- ho output value
              return (BuildLOUDS louds' terminal' label' output')

            stop (BuildLOUDS louds terminal label output) =
              LOUDS
              <$> kl louds
              <*> kt terminal
              <*> kc label
              <*> ko output
  {-# INLINE builder #-}

louds :: StringMap a -> LOUDS a
louds = build . loudsnodes
{-# INLINE louds #-}

data Node =
  Node !Int !Int
  deriving (Eq, Show)

root :: LOUDS a -> Node
root _ = Node 1 0

binarySearch :: (Ord a, GV.Vector v a) => v a -> a -> Int -> Int -> Maybe Int
binarySearch v e = loop
  where
    loop !l !u
      | u <= l = Nothing
      | otherwise = case compare (v `GV.unsafeIndex` k) e of
                      LT -> loop (k + 1) u
                      EQ -> Just k
                      GT -> loop l k
      where
        k = (u + l) `unsafeShiftR` 1
{-# INLINE binarySearch #-}

lookup :: String -> LOUDS a -> Maybe a
lookup s lds@(LOUDS louds terminal _ output) = case lookup' s lds of
  Just (Node i j)
    | terminal ! (i - 1) ->
        Just (output V.! (rank1 terminal (i - 1) - 1)) where
  _ -> Nothing

lookup' :: String -> LOUDS a -> Maybe Node
lookup' s lds@(LOUDS louds  _ label _) =
  go s (root lds)
   where
    go [] n = Just n
    go (w:wx) (Node i j) =
      case binarySearch label (traceShowId w) (traceShowId (fc - 1)) (traceShowId (lc - 1)) of
      Just x | null wx -> Just (Node i j)
             | otherwise ->
               -- | We now know that we have to take (x - i)th child of i
               go wx (Node (x + 1) (i - j))
      Nothing -> Nothing
      where
        -- First child of node i. 1-based.
        fc = select0 louds (i - j) + 1
        lc = select0 louds (i - j + 1)

{-
main :: IO ()
main = do
  let x = SM.fromList [ ("12345", 1 :: Int)
                      , ("12567", 2)
                      , ("23456", 3)
                      , ("3456", 4)
                      ]

--      (children, siblings) = subforest x

--  mapM_ (putStrLn . show) children
--  putStr

  mapM_ (putStrLn . show) (loudsnodes x)

  let LOUDS lds tmn lbl out = louds x

  print out
  print lbl

  print (children (root lds))

  print (lookup "12345" (louds x))
  print (lookup "23456" (louds x))
  print (lookup "3456" (louds x))

  --mapM_ (putStrLn . show) (loudsnodes x)
-}
