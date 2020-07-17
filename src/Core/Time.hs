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
    , getDate     -- :: Date -> IO Text
    , prettyDate  -- :: Date -> Text
    ) where

import Data.Time
    ( Day, DayOfWeek, NominalDiffTime, UTCTime(utctDay), addUTCTime, dayOfWeek
    , fromGregorian, getCurrentTime, nominalDay, toGregorian
    )


-- | Type for specifying exactly which day one wants to see the menu for.
data Date
    = Today
    | Tomorrow
    | Next DayOfWeek  -- ^ This will *always* show the next 'DayOfWeek'
                      --   (e.g. calling 'Next Monday' on a monday will result
                      --   in getting the menu for the following monday)
    | ExactDate Day   -- ^ Manual date entry in the format YYYY-MM-DD
    | ApproxDate (Maybe Integer, Int, Int)  -- ^ Manual date entry in the format
                                            -- DD MM [YYYY]
    deriving (Show)

-- | Pretty print a 'Date'.
prettyDate :: Date -> Text
prettyDate = \case
    ExactDate  d           -> "On " <> tshow d
    ApproxDate (mbY, m, d) ->
        mconcat
            . (["On ", tshow d, " ", tshow (toEnum @Month m)] ++)
            $ maybe [] ((:[]) . tshow) mbY
    otherDate              -> tshow otherDate


-- | Based on a certain weekday, calculate the day.
-- Consistency assumption: We always want to walk forwards in time.
getDate :: Date -> IO Text
getDate = \case
    Today     -> showDay             <$> getCurrentTime
    Tomorrow  -> showDay . addDays 1 <$> getCurrentTime
    Next wday -> do
        t <- getCurrentTime
        let diffToDay = diffBetween wday (dayOfWeek $ utctDay t)
        pure $ showDay (addDays diffToDay t)
    ExactDate d            -> pure $ tshow d
    ApproxDate (mbY, m, d) -> do
        y <- maybe (fst3 . toGregorian . utctDay <$> getCurrentTime) pure mbY
        pure . tshow $ fromGregorian y m d
  where
    -- | Add a specified number of days to a 'UTCTime'.
    addDays :: NominalDiffTime -> UTCTime -> UTCTime
    addDays = addUTCTime . (* nominalDay)

    showDay :: UTCTime -> Text
    showDay = tshow . utctDay

    -- | Some enum hackery.  I don't like this but it's the best I can come up
    -- with right now.
    diffBetween :: DayOfWeek -> DayOfWeek -> NominalDiffTime
    diffBetween d d'
        | d == d'   = 7
        | otherwise = fromIntegral . abs $ (fromEnum d - fromEnum d') `mod` 7

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
