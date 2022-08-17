{- |
   Module      : Util
   Description : Custom prelude for the project
   Copyright   : (c) Tony Zorman  2020 2021 2022
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Util (
  module BasePrelude,
  Text,
  Map,
  eitherOf, -- :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  fi,       -- :: (Integral a, Num b) => a -> b

  -- * Text!
  unwords,  -- :: [Text] -> Text
  words,    -- :: Text -> [Text]
  pack,     -- :: String -> Text
  unpack,   -- :: Text -> String
  tshow,    -- :: Show a => a -> Text
  length,   -- :: Text -> Int
  replace,  -- :: Text -> Text -> Text -> Text
) where

import BasePrelude hiding (length, option, unwords, words)
import Data.Map.Strict (Map)
import Data.Text (Text)

import Data.List qualified as List
import Data.Text qualified as T

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

words :: Text -> [Text]
words = T.words
{-# INLINE words #-}

unwords :: [Text] -> Text
unwords = T.unwords
{-# INLINE unwords #-}

length :: Text -> Int
length = T.length
{-# INLINE length #-}

replace :: Text -> Text -> Text -> Text
replace = T.replace
{-# INLINE replace #-}

pack :: String -> Text
pack = T.pack
{-# INLINE pack #-}

unpack :: Text -> String
unpack = T.unpack
{-# INLINE unpack #-}

-- | Showing text things.
tshow :: Show a => a -> Text
tshow = pack . show
{-# INLINE tshow #-}

-- | Lifting @(||)@ over the Applicative @(->) a@.  See if some
-- predicate holds for at least one of the inputs.
eitherOf :: (a -> Bool) -> (a -> Bool) -> a -> Bool
eitherOf x y = x >>= \x' -> if x' then pure True else y
{-# INLINE eitherOf #-}
