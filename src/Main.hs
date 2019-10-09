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
import Core.Types ( Meal, Mensa, Mensa'(Mensa'), emptyMensa, meals, notes )

-- Other imports
import Control.Applicative      ( liftA2 )
import Control.Concurrent.Async ( Async, wait, withAsync )
import Data.Aeson               ( decode )
import Network.HTTP.Conduit     ( simpleHttp )


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
    -- See note [withAsync]
    withAsync (getMeal zelt)       $ \m1 -> do
     withAsync (getMeal siedepunkt) $ \m2 -> do
      withAsync (getMeal alte)       $ \m3 -> do
          mprint "Heute im Zelt:" m1
          mprint "Heute im Siedepunkt:" m2
          mprint "Heute in der alten Mensa:" m3
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
    m <- decode @Mensa' <$> simpleHttp mensa
    case m of
        Nothing  -> return emptyMensa
        Just men -> return $ getVegOptions men

-- | Filter for the meal options I'm interested in.
getVegOptions :: Mensa' -> Mensa
getVegOptions (Mensa' (mensa : _)) =
    mensa{ meals = filter diet $ meals mensa }
  where
    diet :: Meal -> Bool
    diet = liftA2 (||) veggie vegan . notes
    -- options = getAny . foldMap (Any .) [veggie, vegan] . notes
    veggie = ("vegetarisch" `elem`)
    vegan  = ("vegan"       `elem`)
getVegOptions _ = emptyMensa  -- This should not be able to happen in this context.

-- | Template URL for getting all meals of a certain Mensa.
openMensaURL :: Int -> String
openMensaURL num =
    "https://openmensa.org/api/v2/canteens/" ++ show num ++ "/meals"

{- | Canteens I want to check out.
   FIXME: U-Boot is missing from the API
-}
alte, siedepunkt, zelt :: String
zelt       = openMensaURL 78
alte       = openMensaURL 79
siedepunkt = openMensaURL 82
