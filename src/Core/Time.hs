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
    ( getDate
    ) where

-- Local imports
import Core.CLI (Date(Date, Next, Today, Tomorrow))
import Core.Util (tshow)

-- Text
import Data.Text (Text)

-- Other imports
import Data.Time
    ( Day, NominalDiffTime, UTCTime(utctDay), addUTCTime, dayOfWeek
    , getCurrentTime, nominalDay
    )


-- | Based on a certain weekday, calculate the day.
-- Consistency assumption: We always want to walk forwards in time.
getDate :: Date -> IO Text
getDate = \case
    Today     -> showDay             <$> getCurrentTime
    Tomorrow  -> showDay . addDays 1 <$> getCurrentTime
    Date d    -> pure d
    Next wday -> do
        t <- getCurrentTime
        let diffToDay = diffBetween wday (dayOfWeek $ utctDay t)
        pure $! showDay (addDays diffToDay t)

addDays :: NominalDiffTime -> UTCTime -> UTCTime
addDays i = addUTCTime (i * nominalDay)

showDay :: UTCTime -> Text
showDay = tshow . utctDay

-- | Some enum hackery.  I don't like this but it's the best I can come up with
-- right now.
diffBetween :: (Num c, Enum a, Eq a) => a -> a -> c
{-# SPECIALIZE diffBetween :: Day -> Day -> NominalDiffTime #-}
diffBetween d d'
    | d == d'   = 7
    | otherwise = abs . fromIntegral $ (fromEnum d - fromEnum d') `mod` 7
