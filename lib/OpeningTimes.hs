{- |
  Module      : OpeningTimes
  Description : Query the opening times of all canteens
  Copyright   : (c) Tony Zorman, 2025
  License     : GPL-3
  Maintainer  : Tony Zorman <mail@tony-zorman.com>
-}
module OpeningTimes (printOpeningTimes) where

import CmdLine.Util (wrapText)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding
import Data.Text.IO qualified as T
import Network.HTTP.Client (
  Manager,
  Response (responseBody),
  httpLbs,
  parseRequest_,
  responseBody,
 )
import Text.HTML.Parser (Attr (Attr), Token (TagClose, TagOpen), parseTokens)
import Text.HTML.Parser.Util (between, fromContentText, isContentText, sections, (~==))
import Util

printOpeningTimes :: Manager -> IO ()
printOpeningTimes m = traverse_ (T.putStrLn . (<> "\n") . ppCanteens) =<< queryCanteens m

ppCanteens :: (Text, [[Text]]) -> Text
ppCanteens (name, cs) = mconcat [style name <> "\n", T.intercalate "\n" (map ppLine cs)]
 where
  -- Very manual—and hopefully temporary—printing.
  ppLine :: [Text] -> Text
  ppLine (map strip -> xs) = case xs of
    [] -> ""
    (pref : xs) -> "  + " <> wrapText " " 4 79 (concatMap T.words (pref : xs))

  strip :: Text -> Text
  strip = T.replace "  " " " . T.strip

  style :: Text -> Text
  style s = "\x1b[33m" <> s <> "\x1b[0m"

queryCanteens :: Manager -> IO [(Text, [[Text]])]
queryCanteens m = do
  let r = parseRequest_ "https://www.studentenwerk-dresden.de/mensen/oeffnungszeiten.html"
  tags <- parseTokens . decodeUtf8 . BL.toStrict . responseBody <$> httpLbs r m
  let single :: [Token] -> [[Text]] =
        map (mapMaybe contentText . between startTH (TagClose "tr"))
          . sections (~== startTH)
  let canteens = map (between start (TagClose "div")) $ sections (~== start) tags
  let names = map (head . mapMaybe contentText) canteens
  pure . drop 1 $ zip names (map single canteens)
 where
  contentText :: Token -> Maybe Text
  contentText s = if isContentText s then Just (fromContentText s) else Nothing

  start, startTH :: Token
  start = TagOpen "div" [Attr "class" "col-12"]
  startTH = TagOpen "th" [Attr "class" "w-25", Attr "scope" "row"]
