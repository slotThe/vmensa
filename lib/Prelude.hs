{- |
   Module      : Prelude
   Description : Custom prelude for the project
   Copyright   : (c) Tony Zorman  2020 2021 2022
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Prelude (
  module BasePrelude,
  Text,
  Map,
  eitherOf, -- :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  fi,       -- :: (Integral a, Num b) => a -> b
  wrapWith, -- :: Text -> Int -> Int -> [Text] -> Text

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
eitherOf = liftA2 (||)
{-# INLINE eitherOf #-}

{- | Simple (and probably hilariously inefficient) function to wrap text
at @N@ columns.

NOTE: "Data.Text"s 'Data.Text.length' function is @O(n)@, which may or
      may not matter here.
-}
wrapWith
  :: Text   -- ^ How to concatenate chunks, i.e. the separator
  -> Int    -- ^ Left alignment
  -> Int    -- ^ Max line length (wrap)
  -> [Text] -- ^ Text as chunks that have to stay together
  -> Text   -- ^ Text with line breaks
wrapWith separator al wrapAt chunks
  | wrapAt == 0 = mconcat (intersperse separator chunks)
  | otherwise   = go "" separator al chunks
 where
  go :: Text    -- Already processed part of the text
     -> Text    -- Separator to put between chunks
     -> Int     -- Counter of the current line length
     -> [Text]  -- Text as chunks that have to stay together
     -> Text
  go !done _   !_   []        = done
  go !line sep !acc xs@(c:cs)
    | cLen      >= wrapAt = go goAgain            sep newLen cs
    | al + cLen >= wrapAt = go (goAgain <> ", ")  sep newLen cs
    | combLen   >= wrapAt = go (align line)       sep al     xs
    | otherwise           = go (line <> c <> end) sep newLen cs
   where
    goAgain :: Text = go line " " acc (words c)
    cLen    :: Int  = length c
    combLen :: Int  = acc + cLen            -- Length including the next word
    newLen  :: Int  = combLen + length end  -- Take separator length into account

    -- Nicely left-align the text after a line-break.  We like pretty
    -- things.
    align :: Text -> Text
    align = (<> "\n" <> T.replicate al " ")

    end :: Text
    end = if null cs then "" else sep
