{- |
   Module      : Main
   Description : Connect to the API and filter the results
   Copyright   : (c) Tony Zorman  2019 2020 2021 2022 2025
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Main (main) where

import CLI
import Meal.Options
import Mensa
import Mensa.PP
import OpeningTimes
import Parser.Uhh qualified as Uhh
import Parser.Util
import Time (DatePP (Weekday, Weekend))
import Util

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently), mapConcurrently)
import Data.Aeson (decode')
import Data.Text.IO qualified as T
import Network.HTTP.Client (Manager, httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Fetch all meals, process, format, and print them.
main :: IO ()
main = do
  -- Create a new manager for handling network connections.
  manager <- newManager tlsManagerSettings
  -- Parse command line options.
  execOptionParser >>= \case
    OpeningTimes -> printOpeningTimes manager
    Food(Options{ date, mealOptions, mensaOptions }) -> do
      case date of
        Weekend err -> T.putStrLn err  -- Canteens aren't open on the weekend.
        Weekday d   -> do
          mensen <- do
            let (dd, hh) = canteen mensaOptions
            (\ms -> ppMensen d (mensaOptions{ canteen = ms })) -- Pretty print in bulk
              <$> runConcurrently (
                    (<>) <$> Concurrently (getMensenUhh manager mealOptions hh)
                         <*> Concurrently (getMensenTud manager mealOptions dd)
                  )
          -- Print results synchronously, so as to respect the desired order.
          traverse_ T.putStr mensen

getMensenUhh :: Manager -> MealOptions -> [UhhMensa 'NoMeals] -> IO [Mensa 'Complete]
getMensenUhh _ _ [] = pure []
getMensenUhh manager opts ms@((UhhMensa m _) : _) =
  catch do req  <- parseUrlThrow . unpack . url $ Left m
           tags <- parseTagsUrl req manager
           mapConcurrently (\(UhhMensa m i) ->
                              pure $ addMeals (filterOptions opts (Uhh.parse tags i)) m)
                           ms
        \(_ :: SomeException) -> pure []

-- | Fetch all meals of a given list of TUD canteens and process them.
getMensenTud :: Manager -> MealOptions -> [TudMensa 'NoMeals] -> IO [Mensa 'Complete]
getMensenTud manager opts ms = catMaybes <$> mapConcurrently go ms
 where
  go :: LocMensa 'NoMeals 'DD -> IO (Maybe (Mensa 'Complete))
  go (TudMensa mensa) =
    catch do req  <- parseUrlThrow . unpack . url $ Left mensa
             resp <- httpLbs req manager
             pure . fmap (\ms -> addMeals (filterOptions opts ms) mensa)
                  $ decode' (responseBody resp)
          \(_ :: SomeException) -> pure Nothing
