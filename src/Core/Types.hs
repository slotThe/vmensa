{- |
   Module      : Core.Types
   Description : All types needed for JSON parsing the openmensa API
   Copyright   : (c) Tony Zorman, 2019
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.Types
    ( -- * Types for a 'Mensa' and its meals.
      Mensa(..)
    , Mensa'(..)
    , Meal(..)

    -- * Empty 'Mensa' as a fail-safe.
    , emptyMensa
    )
where

-- Text
import           Data.Text ( Text )
import qualified Data.Text as T

-- Other imports
import Data.Aeson   ( FromJSON )
import Data.List    ( intersperse )
import GHC.Generics ( Generic )


-- | Mensa type for a canteen.
data Mensa = Mensa
    { date   :: Text
    , closed :: Bool    -- ^ This sadly only reports days closed, not hours.
    , meals  :: [Meal]
    } deriving (Generic, FromJSON)

-- | Empty 'Mensa' type.
emptyMensa :: Mensa
emptyMensa = Mensa "" True []

-- | Pretty print only the things I'm interested in.
instance Show Mensa where
    show (Mensa _ c m) = "Closed: " ++ show c ++ lshow m
      where
        -- Show elements of a list with better formatting.  See note [lprint].
        lshow :: [Meal] -> String
        lshow = foldr ((<>) . ("\n\n" <>) . show) ""
        -- lshow = foldl' ((. show) . (<>) . (<> "\n\n")) ""
           {- This is probably going too far, the efficiency gains from using
              'foldl'' are not noticeable here anyways.
           -}

{- Note [lprint]
   ~~~~~~~~~~~~~~~~~~~~~~
   Given a list [a,b], this will print
   @
     a

     b
   @
  instead of
  @
    [a
    b]
  @
  as a normal 'show' would.  The author finds this much easier to read.
-}

-- | The API returns a canteen as a one element array for some reason.
newtype Mensa' = Mensa' [Mensa]
    deriving (Generic, FromJSON)

-- | Type for a meal.
data Meal = Meal
    { id       :: Int
    , name     :: Text
    , category :: Text
    , prices   :: Prices
    , notes    :: [Text]
    } deriving (Generic, FromJSON)

-- | All the different price types.
data Prices = Prices
    { students  :: Double
    , employees :: Double
    , pupils    :: Double
    , others    :: Double
    } deriving (Generic, FromJSON)

-- | Pretty print only the things I'm interested in.
instance Show Meal where
    show Meal{ category, name, notes, prices } =
        T.unpack
            $  name
            <> "\nPreis: "     <> tshow (students prices) <> "€"
            <> "\nNotes: "     <> mconcat (intersperse ", " notes)
            <> "\nKategorie: " <> category
      where
        tshow :: Show a => a -> Text
        tshow = T.pack . show
