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
import Core.CLI
    ( Options(Options, allMeals, lineWrap, onlyDinner, onlyLunch)
    , options
    )
import Core.Types
    ( Meal(category, notes, prices)
    , Mensa(Mensa)
    , Prices(NoPrice)
    , empty
    , showMensa
    )

-- Text
import           Data.Text    ( Text )
import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- Other imports
import Control.Applicative      ( liftA2 )
import Control.Concurrent.Async ( Async, wait, withAsync )
import Data.Aeson               ( decode )
import Data.Foldable            ( traverse_ )
import Data.Monoid              ( All(All, getAll) )
import Data.Time                ( getCurrentTime, utctDay )
import Network.HTTP.Conduit     ( simpleHttp )
import Options.Applicative      ( execParser )


-- | Fetch all meals, procces, format, and print them.
main :: IO ()
main = do
    -- Parse command line options.
    opts@Options{ lineWrap } <- execParser options
    let getMeal' = getMeal opts
    let mprint'  = mprint lineWrap

    -- Get current date in YYYY-MM-DD format.
    d <- tshow . utctDay <$> getCurrentTime

    -- See note [withAsync]
    withAsync (getMeal' $ zelt d)       $ \m1 ->
     withAsync (getMeal' $ siedepunkt d) $ \m2 ->
      withAsync (getMeal' $ alte d)       $ \m3 ->
       withAsync (getMeal' $ uboot d)      $ \m4 -> do
           mprint' "Heute in der alten Mensa:" m3
           mprint' "Heute im U-Boot:" m4
           mprint' "Heute im Zelt:" m1
           mprint' "Heute im Siedepunkt:" m2
  where
    -- | Pretty print an 'Async Mensa' with some prefix string and a line
    -- wrapping limit.
    mprint :: Int -> Text -> Async Mensa -> IO ()
    mprint lw s m = do
        mensa <- wait m
        if empty mensa
            then pure ()
            else traverse_ T.putStrLn
                     [ ""
                     , separator
                     , s
                     , separator
                     , showMensa lw mensa
                     ]

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
-- FIXME: I'm not entirely satisfied with how this is handled.
getMeal :: Options -> Text -> IO Mensa
getMeal Options{ allMeals, onlyDinner, onlyLunch } mensa = do
    men <- decode <$> simpleHttp (T.unpack mensa)
    pure $ case men of
        Nothing -> Mensa []
        Just m  -> getOptions (ifV ++ ifD ++ ifL) m
  where
    ifV = [veggie | not allMeals]
    ifD = [dinner | onlyDinner  ]
    ifL = [lunch  | onlyLunch   ]

    -- | Most of the time we want both.
    veggie :: Meal -> Bool
    veggie = liftA2 (||) vegan vegetarian

    vegetarian :: Meal -> Bool
    vegetarian = ("Menü ist vegetarisch" `elem`) . notes

    vegan :: Meal -> Bool
    vegan = ("Menü ist vegan" `elem`) . notes

    dinner :: Meal -> Bool
    dinner = ("Abendangebot" `T.isInfixOf`) . category

    lunch :: Meal -> Bool
    lunch = not . dinner

-- | Filter for the meal options I'm interested in.
getOptions :: [Meal -> Bool] -> Mensa -> Mensa
getOptions opts (Mensa meals) = Mensa $ filter availableOpts meals
  where
    availableOpts :: Meal -> Bool
    availableOpts = liftA2 (&&) notSoldOut (foldOpts opts)

    notSoldOut :: Meal -> Bool
    notSoldOut = available . prices

    -- | Every predicate should be satisfied in order for the result to be
    -- accepted.
    foldOpts :: [Meal -> Bool] -> Meal -> Bool
    foldOpts os = getAll . foldMap (All .) os

    available :: Prices -> Bool
    available (NoPrice _) = False
    available _           = True

-- | Template URL for getting all meals of a certain Mensa.
openMensaURL
    :: Int   -- ^ Number of the Mensa
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
