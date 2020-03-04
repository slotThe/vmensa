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
    ( -- * Entry-point
      main
    ) where

-- Local imports
import Core.CLI as CLI
    ( MealTime(AllDay, Dinner, Lunch)
    , Options(Options, allMeals, date, lineWrap, mealTime)
    , options
    )
import Core.MealOptions (dinner, filterOptions, lunch, veggie)
import Core.Time (getDate)
import Core.Types
    ( Mensa(Mensa, meals, name, url)
    , empty, mkEmptyMensa, showMeals
    )
import Core.Util (tshow)

-- Text
import           Data.Text    ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- Other imports
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Data.Aeson (decode)
import Data.Foldable (traverse_)
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
    let mprint' = mprint lineWrap (tshow date)

    -- Create new manager for handling network connections.
    manager <- newManager tlsManagerSettings
    let getMensa' = getMensa manager opts

    -- Get specified date in YYYY-MM-DD format.
    d <- getDate date

    -- Build all canteens, decide in which order they will be printed.
    let canteens = map ($! d) [alte, uboot, zelt, siedepunkt]
    -- Connect to the API and parse the necessary JSON.
    mensen <- mapConcurrently getMensa' canteens

    -- Print out the results
    traverse_ mprint' mensen

  where
    -- | Pretty print a 'Mensa'.
    mprint
        :: Int    -- ^ Line Wrap
        -> Text   -- ^ The date when the meals are offered
        -> Mensa
        -> IO ()
    mprint lw d mensa@Mensa{ name, meals } =
        if empty mensa
            then pure ()
            else traverse_ T.putStrLn
                     [ ""
                     , separator
                     , d <> " in: " <> name
                     , separator
                     , showMeals lw meals
                     ]

    -- | Separator for visual separation of different canteens.
    separator :: Text
    separator =
        "=====================================================================\
        \==========="

-- | Fetch all meals of a certain canteen and process them.
getMensa :: Manager -> Options -> Mensa -> IO Mensa
getMensa manager
         Options{ allMeals, mealTime }
         mensa@Mensa{ url }
  = catch
        (do req      <- parseUrlThrow (T.unpack url)
            tryMeals <- decode . responseBody <$> httpLbs req manager

            pure $! case tryMeals of
                Nothing  -> mensa
                Just !ms -> mensa { meals = filterOptions (ifV ++ mt) ms })
        $ handleErrs mensa
  where
    ifV = [veggie | not allMeals]
    mt  = case mealTime of
        AllDay -> []
        Dinner -> [dinner]
        Lunch  -> [lunch]

    -- | If any error occurs, just return the input 'Mensa' (which will be
    -- empty).
    handleErrs :: Mensa -> SomeException -> IO Mensa
    handleErrs m = const (pure m)

{- | Canteens I want to check out.
   Numbers from:
       'https:\/\/api.studentenwerk-dresden.de\/openmensa\/v2\/canteens'
-}
alte, uboot, siedepunkt, zelt :: Text -> Mensa
zelt       = mkEmptyMensa "Mensa Zeltschlößchen" . mensaURL 35
uboot      = mkEmptyMensa "Bio Mensa"            . mensaURL 29
siedepunkt = mkEmptyMensa "Mensa Siedepunkt"     . mensaURL 9
alte       = mkEmptyMensa "Alte Mensa"           . mensaURL 4

-- | Template URL for getting all meals of a certain Meals.
mensaURL
    :: Int   -- ^ Number of the Mensa in the API
    -> Text  -- ^ Date at which we would like to see the food.
    -> Text
mensaURL num date =
    "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
        <> tshow num <> "/days/"
        <> date      <> "/meals"
