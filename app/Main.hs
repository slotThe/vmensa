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

import Core.CLI (Options (Options, canteens, date, lineWrap, noAdds, sections), options)
import Core.MealOptions (filterOptions)
import Core.Mensa (Mensa (Mensa, meals, url), mkMensa, ppMensa)
import Core.Time (DatePP (DatePP, iso, out), getDate)

import qualified Data.Text.IO as T

import Control.Concurrent.Async (forConcurrently)
import Data.Aeson (decode')
import Network.HTTP.Conduit (Manager, httpLbs, newManager, parseUrlThrow, responseBody, tlsManagerSettings)
import Options.Applicative (execParser)


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
  -- Parse command line options.
  opts@Options{ lineWrap, date, canteens, sections, noAdds } <- execParser options

  -- Get the specified date in ISO, as well as a printable format and
  -- create a new manager for handling network connections.
  DatePP{ iso, out } <- getDate date
  manager            <- newManager tlsManagerSettings

  -- See Note [Async]
  mensen <- forConcurrently (mkMensa iso <$> canteens) \mensa ->
    ppMensa lineWrap sections out noAdds <$> getMensa manager opts mensa

  -- Print out the results synchronously, so as to respect the desired
  -- order.
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
getMensa :: Manager -> Options -> Mensa -> IO Mensa
getMensa manager opts mensa@Mensa{ url } =
  catch do req <- parseUrlThrow (unpack url)
           maybe mensa
                 (\ms -> mensa{ meals = filterOptions opts ms })
                 . decode' . responseBody <$> httpLbs req manager
                 -- Strict decoding as we eventually check all fields.
        -- If any error occurs, just return the (empty) input 'Mensa'.
        \(_ :: SomeException) -> pure mensa
