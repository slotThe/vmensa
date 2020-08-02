{- |
   Module      : Main
   Description : Connect to the API and filter the results
   Copyright   : (c) Tony Zorman, 2019, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Main
    ( main  -- :: IO ()
    ) where

import Core.CLI (Options(Options, date, lineWrap), options)
import Core.MealOptions (filterOptions)
import Core.Time (getDate, ppDate)
import Core.Types (Mensa(Mensa, meals, url), mkEmptyMensa, ppMensa)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (decode')
import Network.HTTP.Conduit
    ( Manager, httpLbs, newManager, parseUrlThrow, responseBody
    , tlsManagerSettings
    )
import Options.Applicative (execParser)


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
    -- Parse command line options.
    opts@Options{ lineWrap, date } <- execParser options

    -- Get specified date in YYYY-MM-DD format, then build all canteens and
    -- decide in which order they will be printed.
    d <- getDate date
    let canteens = map ($! d) [alte, uboot, zelt, siedepunkt]

    {- Create new manager for handling network connections, then asynchronously
       connect to the API and parse the necessary JSON.  Note that
       'mapConcurrently' creates a thread for every canteen, though since that
       list is small this is not an issue for us.
    -}
    manager <- newManager tlsManagerSettings
    let ppCanteen :: Mensa -> IO Text
        ppCanteen m = ppMensa lineWrap (ppDate date) <$> getMensa manager opts m
    mensen <- mapConcurrently ppCanteen canteens

    -- Print out the results synchronously.
    traverse_ T.putStr mensen

-- | Fetch all meals of a certain canteen and process them.
getMensa :: Manager -> Options -> Mensa -> IO Mensa
getMensa manager opts mensa@Mensa{ url } = catch
    do req      <- parseUrlThrow (T.unpack url)
       tryMeals <- decode' . responseBody <$> httpLbs req manager
       -- Strict decoding as we eventually check all fields.

       pure $!
           maybe mensa (\ms -> mensa {meals = filterOptions opts ms}) tryMeals

    -- If any error occurs, just return the (empty) input 'Mensa'.
    \(e :: SomeException) -> const (pure mensa) e

-- | Canteens I want to check out.
-- Numbers from: https:\/\/api.studentenwerk-dresden.de\/openmensa\/v2\/canteens
alte, uboot, siedepunkt, zelt :: Text -> Mensa
zelt       = mkEmptyMensa "Mensa Zeltschlösschen" . mensaURL 35
uboot      = mkEmptyMensa "Bio Mensa"             . mensaURL 29
siedepunkt = mkEmptyMensa "Mensa Siedepunkt"      . mensaURL 9
alte       = mkEmptyMensa "Alte Mensa"            . mensaURL 4

-- | Template URL for getting all meals of a certain Meals.
mensaURL
    :: Int   -- ^ Number of the Mensa in the API
    -> Text  -- ^ Date at which we would like to see the food.
    -> Text
mensaURL num date = mconcat
    [ "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
    , tshow num, "/days/"
    , date     , "/meals"
    ]
