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
    , Meal(..)
    , Prices(..)
    )
where

-- Text
import           Data.Text ( Text )
import qualified Data.Text as T

-- Other imports
import Data.Aeson
import Data.List
import GHC.Generics


-- | Mensa type for a canteen.
newtype Mensa = Mensa [Meal]
    deriving (Generic, FromJSON)

-- | Pretty print only the things I'm interested in.
instance Show Mensa where
    show (Mensa m) = lshow m
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

-- | Type for a meal.
data Meal = Meal
    { id       :: Int
    , name     :: Text
    , notes    :: [Text]
    , prices   :: Prices
    , category :: Text
    , image    :: Text
    , url      :: Text
    } deriving (Generic, FromJSON)

-- | All the different price types.
data Prices
    = Prices { students  :: Double
             , employees :: Double
             }
    -- Who at the Studentenwerk thought that this was a good idea?
    | NoPrice [Double]

-- | Manually derive `FromJSON` instance due to dumb field names.
instance FromJSON Prices where
    parseJSON (Object v) = Prices
        <$> v .: "Studierende"
        <*> v .: "Bedienstete"
    parseJSON _ = pure $ NoPrice []

-- | Pretty print only the things I'm interested in.
instance Show Meal where
    show Meal{ category, name, notes, prices } =
        T.unpack
            $  name
            <> "\nPreis: "     <> tshow (mstudents prices)
            <> "\nNotes: "     <> mconcat (intersperse ", " notes)
            <> "\nKategorie: " <> category
      where
        tshow :: (Eq a, Num a, Show a) => a -> Text
        tshow (-1) = "ausverkauft"
        tshow s    = T.pack (show s) <> "€"

        mstudents :: Prices -> Double
        mstudents (Prices  s _) = s
        mstudents (NoPrice _  ) = -1