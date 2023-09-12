{- |
   Module      : Main
   Description : Connect to the API and filter the results
   Copyright   : (c) Tony Zorman  2019 2020 2021 2022
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
import Time (DatePP (Weekday, Weekend))
import Util

import Data.Text.IO qualified as T

import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (decode')
import Network.HTTP.Client (Manager, httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)


-- | Fetch all meals, process, format, and print them.
main :: IO ()
main = do
  -- Parse command line options.
  Options{ date, mealOptions, mensaOptions } <- execOptionParser

  -- Create a new manager for handling network connections.
  manager <- newManager tlsManagerSettings

  case date of
    Weekend err -> T.putStrLn err  -- Canteens aren't open on the weekend.
    Weekday d   -> do
      -- See Note [Async]
      mensen <- (\m -> ppMensen d (mensaOptions{ canteen = m }))
              . catMaybes
            <$> mapConcurrently (getMensa manager mealOptions) (canteen mensaOptions)
      -- Print results synchronously, so as to respect the desired order.
      traverse_ T.putStr mensen

{- Note [Async]
   ~~~~~~~~~~~~~~~~~~~~~~
Here, we concurrently connect to the API, parse the necessary JSON and
then create some pretty-printed text for each canteen.

The function @mapConcurrently@ creates a thread for every canteen, which
would normally be a fire hazard and may cause your laptop to melt
through your desk.  However, since the list of all canteens is rather
small (even trying to show __everything__ there is would only be around
20 network connections), it was deemed "worth it" in this case.
-}

-- | Fetch all meals of a certain canteen and process them.
getMensa :: Manager -> MealOptions -> Mensa 'NoMeals -> IO (Maybe (Mensa 'Complete))
getMensa manager opts mensa =
  catch do req  <- parseUrlThrow . unpack . url $ Left mensa
           resp <- httpLbs req manager
           pure . fmap (\ms -> addMeals (filterOptions opts ms) mensa)
                $ decode' (responseBody resp)
        \(_ :: SomeException) -> pure Nothing
