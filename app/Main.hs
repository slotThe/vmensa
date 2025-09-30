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
import Mensa.PP
import OpeningTimes
import Uhh qualified
import Tud qualified
import Time (DatePP (Weekday, Weekend))
import Util

import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Data.Text.IO qualified as T
import Network.HTTP.Client (newManager)
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
        Weekday d pd -> do
          mensen <- do
            let (dd, hh) = canteen mensaOptions
            (\ms -> ppMensen pd (mensaOptions{ canteen = ms })) -- Pretty print in bulk
              <$> runConcurrently (
                    (<>) <$> Concurrently (Uhh.fetch d manager mealOptions hh)
                         <*> Concurrently (Tud.fetch   manager mealOptions dd)
                  )
          -- Print results synchronously, so as to respect the desired order.
          traverse_ T.putStr mensen
