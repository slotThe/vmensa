{- |
   Module      : Parser.Util
   Description : Utility functions for parsing HTML.
   Copyright   : (c) Tony Zorman, 2025
   License     : GPL-3
   Maintainer  : Tony Zorman <mail@tony-zorman.com>
-}
module Parser.Util (
  parseTagsUrl,
) where

import Util

import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client (Manager, Request, httpLbs, responseBody)
import Text.HTML.Parser qualified as P
import Text.HTML.TagSoup (Tag (..))

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
