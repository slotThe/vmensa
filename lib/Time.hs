{- |
   Module      : Time
   Description : How vmensa knows __when__ to query the cafeterias.
   Copyright   : (c) Tony Zorman  2020 2021 2022 2025
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Time (
  Date (..),    -- instances: Show
  Month (..),   -- instances: Show, Enum
  DatePP (..),  -- isomorphic to Either
  getDate,      -- :: Date -> IO Day
  ppDate,       -- :: Day -> Date -> DatePP
  uhhDate,      -- :: Day -> Day -> Text
) where

import Util

import Data.Time (Day, DayOfWeek (..), NominalDiffTime, UTCTime (..), addUTCTime, dayOfWeek, fromGregorian, getCurrentTime, nominalDay, toGregorian, dayOfWeekDiff)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- | Type for specifying exactly which day one wants to see the menu for.
type Date :: Type
data Date
  = Today
  | Tomorrow
  | Next DayOfWeek
    -- ^ This will __always__ show the next 'DayOfWeek' (e.g. calling
    -- 'Next Monday' on a Monday will result in getting the menu for
    -- the following Monday)
  | ISODate Day
    -- ^ Manual date entry in the format YYYY-MM-DD
  | DMYDate (Int, Maybe Int, Maybe Integer)
    -- ^ Manual date entry in the format DD [MM] [YYYY]
  deriving stock (Show)

-- | A pretty-printed date.
type DatePP :: Type
data DatePP = Weekday Day Text | Weekend Text

-- | Based on a certain weekday, calculate the day.
-- Consistency assumption: We always want to walk forward in time.
getDate :: Date -> IO Day
getDate date = do
  curDay <- utctDay <$> getCurrentTime
  pure case date of
    Today                 -> curDay
    Tomorrow              -> addDays 1         curDay
    Next wday             -> addDays diffToDay curDay
     where diffToDay = diffBetween wday (dayOfWeek curDay)
    ISODate d             -> d
    DMYDate (d, mbM, mbY) -> fromGregorian (fromMaybe y mbY) (fromMaybe m mbM) d
     where (y, m, _) = toGregorian curDay
 where
  -- Some enum hackery.  I don't like this but it's the best I can come
  -- up with right now.
  diffBetween :: DayOfWeek -> DayOfWeek -> NominalDiffTime
  diffBetween d d'
    | d == d'   = 7
    | otherwise = fromIntegral . abs $ (fromEnum d - fromEnum d') `mod` 7

-- | Pretty print a 'Date'.
ppDate :: Day -> Date -> DatePP
ppDate day date = fromMaybe mkDate checkWeekend
 where
  mkDate :: DatePP
  mkDate = Weekday day case date of
    ISODate{} -> mconcat ["On ", tshow day, " (", tshow (dayOfWeek day), ")"]
    DMYDate{} -> mconcat [ "On ", tshow d
                         , " "  , tshow (toEnum @Month m)
                         , " "  , tshow y
                         , " (" , tshow (dayOfWeek day), ")"
                         ]
                 where (y, m, d) = toGregorian day
    otherDate -> tshow otherDate

  -- Fail if the selected day falls on a weekend, as no canteens are
  -- open during that time.[1]
  --
  -- [1]: https://www.studentenwerk-dresden.de/mensen/speiseplan/
  checkWeekend :: Maybe DatePP
  checkWeekend = case dayOfWeek day of
    Saturday -> Just (Weekend warn)
    Sunday   -> Just (Weekend warn)
    _        -> Nothing
   where warn :: Text = "Go home, it's the weekend."

uhhDate :: Day     -- ^ Current date
        -> Day     -- ^ Requested date
        -> Text
uhhDate curDay nextDate =
  let curDw = dayOfWeek curDay
      diff  = fromIntegral (dayOfWeekDiff Friday curDw)
      friday = if curDw > Friday
               then opDays subUTCTime (7 - diff) curDay
               else addDays diff curDay
  in if
    | nextDate == curDay -> "today"
    | nextDate == addDays 1 curDay -> "next_day"
    | nextDate <= friday -> "this_week"
    | dayOfWeek curDay `elem` [Friday, Saturday, Sunday]
      && nextDate <= addDays 7 friday
      -> "next_week"
    | otherwise -> error "huh"

-- | Arbitrary month.
type Month :: Type
data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving stock (Show)

-- | Custom 'Enum' instance that start at 1.
instance Enum Month where
  fromEnum :: Month -> Int
  fromEnum = \case
    January   -> 1
    February  -> 2
    March     -> 3
    April     -> 4
    May       -> 5
    June      -> 6
    July      -> 7
    August    -> 8
    September -> 9
    October   -> 10
    November  -> 11
    December  -> 12

  toEnum :: Int -> Month
  toEnum = \case
    1  -> January
    2  -> February
    3  -> March
    4  -> April
    5  -> May
    6  -> June
    7  -> July
    8  -> August
    9  -> September
    10 -> October
    11 -> November
    12 -> December
    _  -> error "Bad argument to Time.toEnum for Month type"

addDays :: NominalDiffTime -> Day -> Day
addDays = opDays addUTCTime

opDays :: (NominalDiffTime -> UTCTime -> UTCTime) -> NominalDiffTime -> Day -> Day
opDays op n d = utctDay $ (n * nominalDay) `op` UTCTime d 0

subUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
subUTCTime x t = posixSecondsToUTCTime (utcTimeToPOSIXSeconds t - x)
