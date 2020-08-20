{- |
   Module      : Time
   Description : How vmensa knows *when* to query the cafeterias.
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Core.Time
    ( Date(..)    -- instances: Show
    , Month(..)   -- instances: Show, Enum
    , DatePP(..)
    , getDate     -- :: Date -> IO DatePP
    ) where

import Data.Time
    ( Day, DayOfWeek, NominalDiffTime, UTCTime(utctDay), addUTCTime, dayOfWeek
    , fromGregorian, getCurrentTime, nominalDay, toGregorian
    )


-- | Type for specifying exactly which day one wants to see the menu for.
data Date
    = Today
    | Tomorrow
    | Next !DayOfWeek
      -- ^ This will __always__ show the next 'DayOfWeek' (e.g. calling 'Next
      -- Monday' on a Monday will result in getting the menu for the following
      -- Monday)
    | ISODate !Day
      -- ^ Manual date entry in the format YYYY-MM-DD
    | DMYDate !(Maybe Integer, Int, Int)
      -- ^ Manual date entry in the format DD MM [YYYY]
    deriving (Show)

-- | A pretty printed 'Date' in all formats necessary.
data DatePP = DatePP
    { iso :: Text
    , out :: Text
    }

-- | Based on a certain weekday, calculate the day.
-- Consistency assumption: We always want to walk forward in time.
getDate :: Date -> IO DatePP
getDate date = do
    curTime    <- getCurrentTime
    let curDay  = utctDay curTime

    pure . ppDate date $ case date of
        Today               -> curDay
        Tomorrow            -> utctDay $ addDays 1 curTime
        Next wday           ->
            let diffToDay = diffBetween wday (dayOfWeek curDay)
             in utctDay $ addDays diffToDay curTime
        ISODate d           -> d
        DMYDate (mbY, m, d) ->
            let y = fromMaybe (fst3 $ toGregorian curDay) mbY
             in fromGregorian y m d
  where
    -- | Add a specified number of days to a 'UTCTime'.
    addDays :: NominalDiffTime -> UTCTime -> UTCTime
    addDays = addUTCTime . (* nominalDay)

    -- | Some enum hackery.  I don't like this but it's the best I can come up
    -- with right now.
    diffBetween :: DayOfWeek -> DayOfWeek -> NominalDiffTime
    diffBetween d d'
        | d == d'   = 7
        | otherwise = fromIntegral . abs $ (fromEnum d - fromEnum d') `mod` 7

-- | Pretty print a 'Date'.
ppDate :: Date -> Day -> DatePP
ppDate date day = DatePP (tshow day) case date of
    ISODate{} -> mconcat ["On ", tshow day, " (", tshow (dayOfWeek day), ")"]
    DMYDate{} ->
        let (y, m, d) = toGregorian day
         in mconcat [ "On ", tshow d
                    , " "  , tshow (toEnum @Month m)
                    , " "  , tshow y
                    , " (" , tshow (dayOfWeek day), ")"
                    ]
    otherDate -> tshow otherDate

-- | Arbitrary month.
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
    deriving (Show)

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
      _  -> error "Bad argument to Core.Time.toEnum for Month type"
