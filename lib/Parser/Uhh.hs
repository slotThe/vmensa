{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{- |
   Module      : Parser.Uhh
   Description : HTML scraper for the UHH menu.
   Copyright   : (c) Tony Zorman, 2025
   License     : GPL-3
   Maintainer  : Tony Zorman <mail@tony-zorman.com>

Logic copied from https://github.com/cvzi/mensahd/blob/master/hamburg/__init__.py
-}
module Parser.Uhh (parse) where

import Meal
import Util

import Data.Text qualified as T
import Text.HTML.Scalpel
import Text.HTML.TagSoup (Tag (..))
import Data.Time (showGregorian, Day)
import Debug.Trace qualified

-- | @parse ts d is@ parses the canteens with ids @is@ on date @d@ in the list
-- of tags @ts@.
parse :: [Tag Text] -> Day -> [Int] -> [Meals]
parse tags date ids = maybe [] (map (concat . concat)) . flip scrape tags $
  for ids \mensaId ->
    chroots ( "div" @: [ hasClass "tx-epwerkmenu-menu-location-container"
                       , "data-location-id" @= show mensaId
                       ]
            // _class "tx-epwerkmenu-menu-timestamp-wrapper"
            ) do
      d <- attr "data-timestamp" "div"
      Debug.Trace.trace ("d: " <> show d) $ pure ()
      guard $ unpack d == showGregorian date
      chroots (_class "menulist__categorywrapper") do
        category <- stext $ _class "menulist__categorytitle"
        chroots (_class "menue-tile") do
          name <- stext $ _class "singlemeal__headline"
          (notes, prices) <- chroot (_class "singlemeal") do
            notes <- fmap adjustNotes . chroots (_class "singlemeal__icontooltip") $
              T.takeWhile (/= '<') . T.drop (length "<b>") . T.strip <$> attr "title" anySelector
            prices <- do -- XXX trust; could also visit the sibling node instead.
              raw <- stexts (select "dlist » singlemeal__info » singlemeal__info--semibold")
              let pPrice :: Text -> Double
                    = read . unpack . replace "," "." . head . words
                  student  = pPrice (raw !! 0)
                  employee = pPrice (raw !! 1)
              pure $ Just Prices{..}
            pure (notes, prices)
          pure Meal{..}
 where
  adjustNotes :: [Text] -> [Text]
  adjustNotes ns
    | "Vegan" `elem` ns = filter (not . ("laktose" `T.isInfixOf`) . T.toLower) ns
    | otherwise    = ns

_class :: String -> Selector
_class cl = AnyTag @: [hasClass cl]

select :: String -> Selector
select = foldl1' (//) . map (_class . T.unpack) . T.splitOn " » " . pack

stext :: Selector -> ScraperT Text Identity Text
stext = fmap T.strip . text

stexts :: Selector -> ScraperT Text Identity [Text]
stexts = fmap (map T.strip) . texts
