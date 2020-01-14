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
import           Data.Text    ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T

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
    d <- tshow . utctDay <$> getCurrentTime

    -- See note [withAsync]
    withAsync (getMeal $ zelt d)       $ \m1 ->
     withAsync (getMeal $ siedepunkt d) $ \m2 ->
      withAsync (getMeal $ alte d)       $ \m3 ->
       withAsync (getMeal $ uboot d)      $ \m4 -> do
           mprint "Heute im Zelt:" m1
           mprint "Heute im Siedepunkt:" m2
           mprint "Heute in der alten Mensa:" m3
           mprint "Heute im U-Boot:" m4
  where
    -- | Pretty print an 'Async Mensa' with some prefix string.
    mprint :: Text -> Async Mensa -> IO ()
    mprint s m = do
        T.putStrLn ""
        T.putStrLn separator
        T.putStrLn s
        T.putStrLn separator
        print =<< wait m

    -- | Separator for visual separation of different canteens.
    separator :: Text
    separator =
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
getMeal :: Text -> IO Mensa
getMeal mensa = do
    men <- decode  <$> simpleHttp (T.unpack mensa)
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
    -> Text  -- ^ Current date
    -> Text
openMensaURL num date =
    "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
        <> tshow num <> "/days/"
        <> date      <> "/meals"

-- | Canteens I want to check out.
-- Numbers from 'https://api.studentenwerk-dresden.de/openmensa/v2/canteens'
alte, uboot, siedepunkt, zelt :: Text -> Text
zelt       = openMensaURL 35
uboot      = openMensaURL 29
siedepunkt = openMensaURL 9
alte       = openMensaURL 4

-- | Helper function for showing things.
tshow :: Show a => a -> Text
tshow = T.pack . show
