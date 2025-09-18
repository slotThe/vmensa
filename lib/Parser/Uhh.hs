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

-- | @parse ts i@ parses the mensa with id @i@ in @ts@.
parse :: [Tag Text] -> Int -> [Meal]
parse tags mensaId = concat . fromMaybe [[]] . flip scrape tags $
  chroots ( "div" @: [hasClass "tx-epwerkmenu-menu-location-container", "data-location-id" @= show mensaId]
         // select "tx-epwerkmenu-menu-timestamp-wrapper » menulist__categorywrapper"
         ) do
    category <- stext $ _class "menulist__categorytitle"
    chroots (_class "menue-tile") do
      name <- stext $ _class "singlemeal__headline"
      (notes, prices) <- chroot (_class "singlemeal") do
        notes <- chroots (_class "singlemeal__icontooltip") do
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

_class :: String -> Selector
_class cl = AnyTag @: [hasClass cl]

select :: String -> Selector
select = foldl1' (//) . map (_class . T.unpack) . T.splitOn " » " . pack

stext :: Selector -> ScraperT Text Identity Text
stext = fmap T.strip . text

stexts :: Selector -> ScraperT Text Identity [Text]
stexts = fmap (map T.strip) . texts
