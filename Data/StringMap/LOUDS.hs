{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.StringMap.LOUDS (
    LOUDS
  , louds
  , root
  , lookup
  , foldWithKey
  , toList
  ) where

import           Data.StringMap.Base         as SM hiding (lookup, null, (!), child, size, foldWithKey, toList, size)

import           Succinct.Dictionary.Builder
import           Succinct.Dictionary.Class
import           Succinct.Dictionary.Poppy
import Succinct.Tree.LOUDS (Zipper(..), children, root, parent)

import Data.Foldable hiding (toList)
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

-- | Bogus wrapper to hide the non standard `Buildable` instance.
newtype LOUDS' a = LOUDS' { unLOUDS :: (LOUDS a) }

instance Functor LOUDS where
  fmap f (LOUDS louds terminal label output) =
    LOUDS louds terminal label (fmap f output)
  {-# INLINE fmap #-}

instance Foldable LOUDS where
  foldMap f (LOUDS _ _ _ a) = foldMap f a
  {-# INLINE foldMap #-}

instance Traversable LOUDS where
  traverse f (LOUDS louds terminal label output) =
    LOUDS louds terminal label <$> traverse f output
  {-# INLINE traverse #-}

instance Access Bool (LOUDS a) where
  size (LOUDS l _ _ _) = size l
  {-# INLINE size #-}
  (!) (LOUDS l _ _ _) i = l ! i
  {-# INLINE (!) #-}

instance Dictionary Bool (LOUDS a) where
  rank a (LOUDS l _ _ _) i = rank a l i
  {-# INLINE rank #-}
  select a (LOUDS l _ _ _) i = select a l i
  {-# INLINE select #-}

instance Select0 (LOUDS a) where
  select0 (LOUDS l _ _ _)i = select0 l i
  {-# INLINE select0 #-}

instance Buildable (StringMap a) (LOUDS' a) where
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

            step (BuildLOUDS louds terminal label output) (Branch sym (Leaf value) _)  = do
              louds' <- hl louds True
              terminal' <- ht terminal True
              label' <- hc label sym
              output' <- ho output value
              return (BuildLOUDS louds' terminal' label' output')

            step (BuildLOUDS louds terminal label output) (Branch sym _ _)  = do
              louds' <- hl louds True
              terminal' <- ht terminal False
              label' <- hc label sym
              return (BuildLOUDS louds' terminal' label' output)

            step (BuildLOUDS louds terminal label output) _  = do
              return (BuildLOUDS louds terminal label output)

            stop (BuildLOUDS louds terminal label output) = do
              l <- LOUDS
                   <$> kl louds
                   <*> kt terminal
                   <*> kc label
                   <*> ko output
              return (LOUDS' l)
  {-# INLINE builder #-}

louds :: StringMap a -> LOUDS a
louds = unLOUDS . build . loudsnodes
{-# INLINE louds #-}

traverseChild :: Char -> Zipper (LOUDS a) -> Maybe (Zipper (LOUDS a))
traverseChild w node = go (children node)
  where
    go [] = Nothing
    go (Zipper i j lds@(LOUDS _ _ label _):nx)
      | label P.! (i - 1) == w = Just (Zipper i j lds)
      | otherwise              = go nx
{-# INLINE traverseChild #-}

-- | Takes a LOUDS-node and traverses the nodes according to a given string.
traverse' :: String -> Zipper (LOUDS a) -> Maybe (Zipper (LOUDS a))
traverse' = go
  where
    go [] node     = Just node
    go (w:wx) node =
      case traverseChild w node of
        Just node' -> go wx node'
        Nothing    -> Nothing
{-# INLINE traverse' #-}

-- | Lookup a complete string in a LOUDS-trie.
lookup :: String -> LOUDS a -> Maybe a
lookup s lds@(LOUDS _ terminal _ output) =
  case traverse' s (root lds) of
    Just (Zipper i j _)
      | terminal ! (i - 1) -> Just ( output V.! (rank1 terminal (i - 1)) )
    _                      -> Nothing
{-# INLINE lookup #-}

rfold' :: (String -> a -> b -> b) -> b -> (Key -> Key) -> Zipper (LOUDS a) -> b
rfold' f r' k0 node = go k0 r' (children node)
  where
    go kf r [] = r
    go kf r (n@(Zipper i j (LOUDS _ terminal label output)):nx)
      | isTerminal = let r''  = go kf r nx
                         r''' = f ((kf . ((label P.! (i - 1)) :)) []) value r''
                     in rfold' f r''' (kf . ((label P.! (i - 1)) :)) n
      | otherwise  =
          let r'' = go kf r nx in rfold' f r'' (kf . ((label P.! (i - 1)) :)) n
      where
        isTerminal = terminal ! (i - 1)
        value = output V.! (rank1 terminal (i - 1))
{-# INLINE rfold' #-}

foldWithKey :: (String -> a -> b -> b) -> b -> Zipper (LOUDS a) -> b
foldWithKey f e = rfold' f e id
{-# INLINE foldWithKey #-}

toList :: Zipper (LOUDS a) -> [(String, a)]
toList = foldWithKey (\k v r -> (k, v): r) []
{-# INLINE toList #-}

mergeWithKey :: (String -> a -> b -> Maybe c)
             -> (LOUDS a -> LOUDS c)
             -> (LOUDS b -> LOUDS c)
             -> LOUDS a
             -> LOUDS b
             -> LOUDS c
mergeWithKey f g1 g2 a b = go (root a) (root b)
  where
    go z1 z2 = undefined
