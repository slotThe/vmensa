{- |
   Module      : Tud
   Description : TUD-specific functions.
   Copyright   : (c) Tony Zorman, 2025
   License     : GPL-3
   Maintainer  : Tony Zorman <mail@tony-zorman.com>
-}
module Tud (
  url,
  fetch,
  addDate,
  canteens,
) where

import Meal.Options
import Mensa qualified as M
import Mensa (Loc (..), Mensa, MensaState (..), TudMensa, addMeals, asMensa, mapMensa)
import Util

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (decode')
import Data.Time
import Network.HTTP.Client (Manager, httpLbs, parseUrlThrow, responseBody)

url :: Int -> Text -> Text
url num date = mconcat
  [ "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
  , tshow num , "/days/"
  , date      , "/meals"
  ]

-- | Fetch all meals of a given list of TUD canteens and process them.
fetch :: Manager -> MealOptions -> [TudMensa 'NoMeals] -> IO [Mensa 'Complete]
fetch manager opts ms = catMaybes <$> mapConcurrently go (map asMensa ms)
 where
  go :: Mensa 'NoMeals -> IO (Maybe (Mensa 'Complete))
  go mensa =
    catch do req  <- parseUrlThrow . unpack . M.url $ Left mensa
             resp <- httpLbs req manager
             pure . fmap (\ms -> addMeals (filterOptions opts ms) mensa)
                  $ decode' (responseBody resp)
          \(_ :: SomeException) -> pure Nothing

-- | Add a missing date to a canteen.
addDate :: Day -> TudMensa 'Incomplete -> TudMensa 'NoMeals
addDate reqDay = mapMensa (M.addDate (tshow reqDay))

canteens :: [(Int, (Loc, Text, [Text]))]
canteens = map (\(a,(b,c)) -> (a,(DD,b,c)))
    [ (4,   ("Alte Mensa"                     , ["Alt"]))
    , (6,   ("Mensa Matrix"                   , ["Re", "Mat"]))
    , (8,   ("Mensologie"                     , ["Me"]))
    , (9,   ("Mensa Siedepunkt"               , ["Si"]))
    , (10,  ("Mensa TellerRandt"              , ["T"]))
    , (11,  ("Mensa Palucca Hochschule"       , ["Pal", "Ho"]))
    , (13,  ("Mensa Stimm-Gabel"              , ["Sti", "Ga"]))
    , (24,  ("Mensa Kraatschn"                , ["K"]))
    , (25,  ("Mensa Mahlwerk"                 , ["Mah"]))
    , (28,  ("MiO - Mensa im Osten"           , ["MiO", "Os"]))
    , (29,  ("BioMensa U-Boot"                , ["Bio", "U"]))
    , (30,  ("Mensa Sport"                    , ["Sport"]))
    , (32,  ("Mensa Johannstadt"              , ["Jo"]))
    , (33,  ("Mensa WUeins / Sportsbar"       , ["W", "Sports"]))
    , (34,  ("Mensa Brühl"                    , ["Br"]))
    , (35,  ("Zeltschlösschen"                , ["Zel"]))
    , (36,  ("Grill Cube"                     , ["Gr", "Cu"]))
    , (37,  ("Pasta-Mobil"                    , ["Pas", "Mo"]))
    , (38,  ("Mensa Rothenburg"               , ["Ro"]))
    , (39,  ("Mensa Bautzen Polizeihochschule", ["Ba", "Po"]))
    , (42,  ("Mensa Oberschmausitz"           , ["Ob"]))
    ]
