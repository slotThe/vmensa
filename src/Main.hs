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
import Core.MealOptions (dinner, getOptions, lunch, veggie)
import Core.CLI
    ( MealTime(AllDay, Dinner, Lunch)
    , Options(Options, allMeals, lineWrap, mealTime)
    , options
    )
import Core.Types
    ( Mensa(Mensa, meals, name, url)
    , empty
    , mkEmptyMensa
    , showMeals
    )

-- Text
import           Data.Text    ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- Other imports
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, catch)
import Data.Aeson (decode)
import Data.Foldable (traverse_)
import Data.Time (getCurrentTime, utctDay)
import Network.HTTP.Conduit
    ( Manager, httpLbs, newManager, parseUrlThrow, responseBody
    , tlsManagerSettings
    )
import Options.Applicative (execParser)


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
    -- Parse command line options.
    opts@Options{ lineWrap } <- execParser options
    let mprint'  = mprint lineWrap

    -- Create new manager for handling network connections.
    manager <- newManager tlsManagerSettings
    let getMeal' = getMeal manager opts

    -- Get current date in YYYY-MM-DD format.
    d <- tshow . utctDay <$> getCurrentTime

    -- Connect to the API and parse the necessary JSON.
    mensen <-
        mapConcurrently getMeal' $ map ($ d) [alte, uboot, zelt, siedepunkt]

    -- Print out the results
    traverse_ mprint' mensen

  where
    -- | Pretty print an 'Async Mensa' with some prefix string and a line
    -- wrapping limit.
    mprint :: Int -> Mensa -> IO ()
    mprint lw mensa@Mensa{ name, meals } =
        if empty mensa
            then pure ()
            else traverse_ T.putStrLn
                     [ ""
                     , separator
                     , "Heute in: " <> name
                     , separator
                     , showMeals lw meals
                     ]

    -- | Separator for visual separation of different canteens.
    separator :: Text
    separator =
        "=====================================================================\
        \==========="

-- | Fetch all meals of a certain canteen and process them.
getMeal :: Manager -> Options -> Mensa -> IO Mensa
getMeal manager
        Options{ allMeals, mealTime }
        mensa@Mensa{ url }
  = catch
        (do req      <- parseUrlThrow (T.unpack url)
            tryMeals <- decode . responseBody <$> httpLbs req manager

            pure $ case tryMeals of
                Nothing -> mensa
                Just ms -> mensa { meals = getOptions (ifV ++ mt) ms })
        $ handleErrs mensa
  where
    ifV = [veggie | not allMeals]
    mt  = case mealTime of
        AllDay -> []
        Dinner -> [dinner]
        Lunch  -> [lunch]

    handleErrs :: Mensa -> SomeException -> IO Mensa
    handleErrs m = const (pure m)

-- | Template URL for getting all meals of a certain Meals.
mensaURL
    :: Int   -- ^ Number of the Meals
    -> Text  -- ^ Current date
    -> Text
mensaURL num date =
    "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
        <> tshow num <> "/days/"
        <> date      <> "/meals"

-- | Canteens I want to check out.
-- Numbers from 'https://api.studentenwerk-dresden.de/openmensa/v2/canteens'
alte, uboot, siedepunkt, zelt :: Text -> Mensa
zelt       = mkEmptyMensa "Mensa Zeltschlößchen" . mensaURL 35
uboot      = mkEmptyMensa "Bio Mensa"            . mensaURL 29
siedepunkt = mkEmptyMensa "Mensa Siedepunkt"     . mensaURL 9
alte       = mkEmptyMensa "Alte Mensa"           . mensaURL 4

-- | Helper function for showing things.
tshow :: Show a => a -> Text
tshow = T.pack . show
