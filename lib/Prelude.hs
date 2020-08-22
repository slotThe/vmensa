{- |
   Module      : Prelude
   Description : Custom prelude for the project
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Prelude
    ( module BasePrelude
    , Text
    , Map
    , fromList
    , toList

    , pack      -- :: String -> Text
    , unpack    -- :: Text -> String
    , tshow     -- :: Show a => a -> Text
    , fst3      -- :: (a, b, c) -> a
    , eitherOf  -- :: (a -> Bool) -> (a -> Bool) -> a -> Bool
    ) where

import BasePrelude hiding (option, toList)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Exts (fromList, toList)

import qualified Data.Text as T


pack :: String -> Text
pack = T.pack

unpack :: Text -> String
unpack = T.unpack

-- | Showing text things.
tshow :: Show a => a -> Text
tshow = pack . show
{-# INLINE tshow #-}

-- | Get the first element out of a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | Lifting @(||)@ over the Applicative @(->) a@.  See if some predicate holds
-- for at least one of the inputs.
eitherOf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
eitherOf = liftA2 (||)
