{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
{- |
   Module      : Uhh
   Description : UHH-specific functions.
   Copyright   : (c) Tony Zorman, 2025
   License     : GPL-3
   Maintainer  : Tony Zorman <mail@tony-zorman.com>
-}
module Uhh (
  fetch,
  addDate,
  parse,
  canteens,
) where

import Meal
import Meal.Options
import Mensa qualified as M
import Mensa (Loc (..), LocMensa (..), Mensa, MensaState (..), UhhMensa, addMeals, asMensa, mapMensa, url)
import Time
import Util hiding (id)

import Network.HTTP.Client (Manager, parseUrlThrow)
import Data.Text qualified as T
import Text.HTML.Scalpel
import Text.HTML.TagSoup (Tag (..))
import Data.Time hiding (addDays)

fetch :: Day -> Manager -> MealOptions -> [UhhMensa 'NoMeals] -> IO [Mensa 'Complete]
fetch _ _ _ [] = pure []
fetch date manager opts ms@(m : _) =
  catch do req  <- parseUrlThrow . unpack . url . Left . asMensa $ m
           tags <- parseTagsUrl req manager
           pure $ zipWith (\m meals -> addMeals (filterOptions opts meals) (asMensa m))
                          ms
                          (Uhh.parse tags date (map _id ms))
        \(_ :: SomeException) -> pure []
 where
  _id :: UhhMensa s -> Int
  _id (UhhMensa _ i) = i

-- | Add a missing date to a canteen.
addDate :: Day -> Day -> UhhMensa 'Incomplete  -> UhhMensa 'NoMeals
addDate curDay reqDay = mapMensa (M.addDate (uhhDate curDay reqDay))
 where
  uhhDate :: Day  -- Current date
          -> Day  -- Requested date
          -> Text
  uhhDate curDay nextDate =
    let curDw = dayOfWeek curDay
        diff  = fromIntegral (dayOfWeekDiff Friday curDw)
        fri   = if curDw > Friday
                then opDays subUTCTime (7 - diff) curDay
                else addDays diff curDay
    in if
      | nextDate == curDay -> "today"
      | nextDate == addDays 1 curDay -> "next_day"
      | nextDate <= fri -> "this_week"
      | dayOfWeek curDay `elem` [Friday, Saturday, Sunday]
        && nextDate <= addDays 7 fri
        -> "next_week"
      | otherwise -> error "huh"

-- | @parse ts d is@ parses the canteens with ids @is@ on date @d@ in the list
-- of tags @ts@.
--
-- Logic copied from https://github.com/cvzi/mensahd/blob/master/hamburg/__init__.py
parse :: [Tag Text] -> Day -> [Int] -> [Meals]
parse tags (showGregorian -> date) ids =
  maybe [] (map (concat . concat)) . flip scrape tags $ for ids \mensaId ->

    chroots ( "div" @: [ hasClass "tx-epwerkmenu-menu-location-container"
                       , "data-location-id" @= show mensaId
                       ]
            // _class "tx-epwerkmenu-menu-timestamp-wrapper"
            ) do
      guard . (date ==) . unpack =<< attr "data-timestamp" "div" -- for multi-day views
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

canteens :: [(Int, (Loc, Text, [Text]))]
canteens = map (\(a,(b,c)) -> (a,(HH,b,c)))
    [ (137, ("Mensa Studierendenhaus"   , ["Stu"]))
    , (142, ("Mensa Blattwerk"          , ["Bl"]))
    , (143, ("Schlüters (Pizza & More)" , ["Sc"]))
    , (148, ("Café dell'Arte"           , ["D"]))
    , (151, ("Mensa Geomatikum"         , ["Ge"]))
    , (154, ("Mensa Philturm"           , ["Ph"]))
    , (156, ("Mensa Botanischer Garten" , ["Bo"]))
    , (158, ("Mensa Harbug TU"          , ["Ha"]))
    , (161, ("Mensa Stellingen"         , ["Ste"]))
    , (162, ("Mensa Bucerius Law School", ["Buc", "L"]))
    , (164, ("Mensa Finkenau"           , ["Fi"]))
    , (166, ("Mensa HCU HafenCity"      , ["HCU"]))
    , (168, ("Mensa Bergedorf"          , ["Berg"]))
    , (170, ("Mensa Berliner Tor"       , ["Berl"]))
    , (175, ("Café Jungiusstraße"       , ["J"]))
    , (176, ("Café Alexanderstraße"     , ["Ale"]))
    , (177, ("Café CFEL"                , ["CF"]))
    , (178, ("Café am Mittelweg"        , ["Mit"]))
    , (179, ("Campus Food Truck"        , ["Fo"]))
    , (383, ("Café ZessP TU"            , ["Zes"]))
    ]
