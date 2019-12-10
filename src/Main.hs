{- |
   Module      : Main
   Description : Connect to the API and filter the results
   Copyright   : (c) Tony Zorman, 2019
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
import Core.Types ( Meal, Mensa(Mensa), Prices(NoPrice), notes, prices )

-- Text
import Data.Text ( Text )

-- Other imports
import Control.Applicative      ( liftA2 )
import Control.Concurrent.Async ( Async, wait, withAsync )
import Data.Aeson               ( decode )
import Data.Time                ( getCurrentTime, utctDay )
import Network.HTTP.Conduit     ( simpleHttp )


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
    -- Get current date in YYYY-MM-DD format.
    d <- show . utctDay <$> getCurrentTime

    -- See note [withAsync]
    withAsync (getMeal $ zelt d)       $ \m1 -> do
     withAsync (getMeal $ siedepunkt d) $ \m2 -> do
      withAsync (getMeal $ alte d)       $ \m3 -> do
       withAsync (getMeal $ uboot d)      $ \m4 -> do
           mprint "Heute im Zelt:" m1
           mprint "Heute im Siedepunkt:" m2
           mprint "Heute in der alten Mensa:" m3
           mprint "Heute im U-Boot:" m4
  where
    -- | Pretty print an 'Async Mensa' with some prefix string.
    mprint :: String -> Async Mensa -> IO ()
    mprint s m = do
        putStrLn ""
        printSeparator
        putStrLn s
        printSeparator
        print =<< wait m

    -- | Separator for visual separation of different canteens.
    printSeparator :: IO ()
    printSeparator = putStrLn
        "=====================================================================\
        \==========="

{- Note [withAsync]
   ~~~~~~~~~~~~~~~~~~~~~~
   'withAsync' is like 'async', except that the 'Async' is automatically killed
   (using 'uninterruptibleCancel') if the enclosing 'IO' operation returns
   before it has completed.  This essentially makes 'withAsync' exception safe.
   It does result in slighly uglier syntax, but, alas, such is life.
-}

-- | Fetch all meals of a certain canteen and process them.
getMeal :: String -> IO Mensa
getMeal mensa = do
    men <- decode @Mensa <$> simpleHttp mensa
    pure $ case men of
        Nothing -> Mensa []
        Just m  -> getVegOptions m

-- | Filter for the meal options I'm interested in.
getVegOptions :: Mensa -> Mensa
getVegOptions (Mensa meals) = Mensa $ filter options meals
  where
    options :: Meal -> Bool
    options = liftA2 (&&) notSoldOut diet

    notSoldOut :: Meal -> Bool
    notSoldOut = available . prices

    diet :: Meal -> Bool
    diet = liftA2 (||) veggie vegan . notes

    available :: Prices -> Bool
    available (NoPrice _) = False
    available _           = True

    veggie :: [Text] -> Bool
    veggie = ("Menü ist vegetarisch" `elem`)

    vegan :: [Text] -> Bool
    vegan  = ("Menü ist vegan" `elem`)

-- | Template URL for getting all meals of a certain Mensa.
openMensaURL
    :: Int     -- ^ Number of the Mensa
    -> String  -- ^ Current date
    -> String
openMensaURL num date =
    "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
        ++ show num ++ "/days/"
        ++ date     ++ "/meals"

-- | Canteens I want to check out.
-- Numbers from 'https://api.studentenwerk-dresden.de/openmensa/v2/canteens'
alte, uboot, siedepunkt, zelt :: String -> String
zelt       = openMensaURL 35
uboot      = openMensaURL 29
siedepunkt = openMensaURL 9
alte       = openMensaURL 4
