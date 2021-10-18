{- |
   Module      : Main
   Description : Connect to the API and filter the results
   Copyright   : (c) Tony Zorman  2019 2020 2021
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Main (
  main, -- :: IO ()
) where

import CLI
import Meal.Options
import Mensa
import Time

import qualified Data.Text.IO as T

import Control.Concurrent.Async (forConcurrently)
import Data.Aeson (decode')
import Network.HTTP.Client (Manager, httpLbs, newManager, parseUrlThrow, responseBody)
import Network.HTTP.Client.TLS (tlsManagerSettings)


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
  -- Parse command line options.
  Options{ date = DatePP{ iso, out }, mealOptions, mensaOptions } <- execOptionParser

  -- Create a new manager for handling network connections.
  manager <- newManager tlsManagerSettings

  -- See Note [Async]
  mensen <- forConcurrently (mkMensa iso <$> canteen mensaOptions) \mensa ->
    (\m -> ppMensa out mensaOptions{ canteen = m }) <$>
      getMensa manager mealOptions mensa

  -- Print results synchronously, so as to respect the desired order.
  traverse_ T.putStr mensen

{- Note [Async]
   ~~~~~~~~~~~~~~~~~~~~~~
Here, we concurrently connect to the API, parse the necessary JSON and
then create some pretty-printed text for each canteen.

The function @forConcurrently@ creates a thread for every canteen, which
would normally be a fire hazard and may cause your laptop to melt
through your desk.  However, since the list of all canteens is rather
small (even trying to show __everything__ there is would only be around
20 network connections), it was deemed "worth it" in this case.
-}

-- | Fetch all meals of a certain canteen and process them.
getMensa :: Manager -> MealOptions -> Mensa -> IO Mensa
getMensa manager opts mensa@Mensa{ url } =
  catch do req <- parseUrlThrow (unpack url)
           maybe mensa
                 (\ms -> mensa{ meals = filterOptions opts ms })
                 . decode' . responseBody <$> httpLbs req manager
                 -- Strict decoding as we eventually check all fields.
        -- If any error occurs, just return the (empty) input 'Mensa'.
        \(_ :: SomeException) -> pure mensa
