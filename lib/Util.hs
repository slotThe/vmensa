{- |
   Module      : Util
   Description : Custom prelude for the project
   Copyright   : (c) Tony Zorman  2020 2021 2022 2025
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Util (
  module BasePrelude,
  Type,
  Text,
  Map,
  eitherOf, -- :: (a -> Bool) -> (a -> Bool) -> a -> Bool
  fi,       -- :: (Integral a, Num b) => a -> b
  snd3,     -- :: (a, b, c) -> b
  parseTagsUrl,

  -- * Text!
  unwords,  -- :: [Text] -> Text
  words,    -- :: Text -> [Text]
  pack,     -- :: String -> Text
  unpack,   -- :: Text -> String
  tshow,    -- :: Show a => a -> Text
  length,   -- :: Text -> Int
  replace,  -- :: Text -> Text -> Text -> Text
) where

import BasePrelude hiding (length, unwords, words)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (Manager, Request, httpLbs, responseBody)
import Text.HTML.Parser qualified as P
import Text.HTML.TagSoup (Tag (..))

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

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b
{-# INLINE snd3 #-}

parseTagsUrl :: Request -> Manager -> IO [Tag Text]
parseTagsUrl req manager =
  catch (map tokenToTag . P.parseTokens . decodeUtf8 . BL.toStrict . responseBody
           <$> httpLbs req manager)
        \(_ :: SomeException) -> pure []

-- | Convert a @html-parse@ 'P.Token' into a @tagsoup@ 'Tag'.
tokenToTag :: P.Token -> Tag Text
tokenToTag = \case
  P.TagOpen t as      -> TagOpen t (map (\(P.Attr a b) -> (a, b)) as)
  P.TagClose t        -> TagClose t
  P.ContentText t     -> TagText t
  P.ContentChar t     -> TagText (pack [t])
  P.Comment{}         -> TagComment ""          -- not needed
  P.Doctype{}         -> TagComment ""          -- not needed
  P.TagSelfClose t as -> TagOpen t (map (\(P.Attr a b) -> (a, b)) as)
