-- ----------------------------------------------------------------------------

{- |
  Module     : Data.StringMap.Lazy
  Copyright  : Copyright (C) 2009-2014 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de), Sebastian Philipp (sebastian@spawnhost.de)
  Stability  : experimental
  Portability: not portable

  An efficient implementation of maps from strings to arbitrary values.

  Values can associated with an arbitrary byte key. Searching for keys is very fast, but
  the prefix tree probably consumes more memory than "Data.Map". The main differences are the special
  'prefixFind' functions, which can be used to perform prefix queries. The interface is
  heavily borrowed from "Data.Map" and "Data.IntMap".

  Most other function names clash with "Prelude" names, therefore this module is usually
  imported @qualified@, e.g.

  > import Data.StringMap (StringMap)
  > import qualified Data.StringMap as T

  Many functions have a worst-case complexity of /O(min(n,L))/. This means that the operation
  can become linear with the number of elements with a maximum of /L/, the length of the
  key (the number of bytes in the list). The functions for searching a prefix have a worst-case
  complexity of /O(max(L,R))/. This means that the operation can become linear with
  /R/, the number of elements found for the prefix, with a minimum of /L/.

  The module exports include the internal data types, their constructors and access
  functions for ultimate flexibility. Derived modules should not export these
  (as shown in "Holumbus.Data.StrMap") to provide only a restricted interface.

-}

-- ----------------------------------------------------------------------------

module Data.StringMap.Lazy
    (
    -- * Map type
    StringMap() -- (..) I don't think we should export the constructor.
    , Key

    -- * Operators
    , (!)

    -- * Query
    , value
    , valueWithDefault
    , null
    , size
    , member
    , lookup
    , findWithDefault
    , prefixFind
    , prefixFindWithKey
    , prefixFindWithKeyBF
    , lookupRange

    -- * Construction
    , empty
    , singleton

    -- ** Insertion
    , insert
    , insertWith
    , insertWithKey

    -- ** Delete\/Update
    , delete
    , update
    , updateWithKey

    -- * Combine
    -- ** Union
    , union
    , unionWith
    , unionMapWith
    , unionWithKey

    -- ** Difference
    , difference
    , differenceWith
    , differenceWithKey

    -- ** Interset
    , intersection
    , intersectionWith

    -- * Traversal
    -- ** Map
    , map
    , mapWithKey
    , mapM
    , mapWithKeyM
    , mapMaybe

    -- * Folds
    , fold
    , foldWithKey

    -- * Conversion
    , keys
    , elems

    -- ** Lists
    , fromList
    , toList
    , toListShortestFirst

    -- ** Maps
    , fromMap
    , toMap

    -- * Prefix and Fuzzy Search
    , prefixFilter     -- fuzzy search
    , prefixFilterNoCase
    , lookupNoCase
    )
where

import           Data.StringMap.Base
import           Data.StringMap.FuzzySearch
import           Prelude                    ()

-- ----------------------------------------------------------------------------

