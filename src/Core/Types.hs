{-# LANGUAGE InstanceSigs #-}

{- |
   Module      : Core.Types
   Description : All types needed for JSON parsing the openmensa API
   Copyright   : (c) Tony Zorman, 2019, 2020
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
    , showMensa
    )
where

-- Text
import           Data.Text ( Text )
import qualified Data.Text as T

-- Other imports
import Control.Applicative ( (<|>) )
import Data.Aeson          ( FromJSON(parseJSON), Value(Object), (.:) )
import Data.List           ( intersperse )
import GHC.Generics        ( Generic )


-- | Mensa type for a canteen.
newtype Mensa = Mensa [Meal]
    deriving (Generic, FromJSON)

-- | Type for a meal.
data Meal = Meal
    { id       :: Int
    , name     :: Text
    , notes    :: [Text]
    , prices   :: Prices
    , category :: Text
    , image    :: Text  -- ^ Sadly not an image of the actual food :(
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
        <$> (v .: "Studierende" <|> v .: "Preis 1")
        <*> (v .: "Bedienstete" <|> v .: "Preis 2")
    parseJSON _ = pure $ NoPrice []

{- | Pretty print only the things I'm interested in.
   This is not a show instance because printing text is faster than printing a
   string.
   See Note [pretty printing]
-}
showMensa :: Mensa -> Text
showMensa (Mensa m) = T.init . T.unlines . map showMeal $ m
  where
    {- | Pretty print only the things I'm interested in.
       This is not a show instance because printing text is faster than printing a
       string.
    -}
    showMeal :: Meal -> Text
    showMeal Meal{ category, name, notes, prices } =
           "\n"
        <> name
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

{- Note [pretty printing]
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
