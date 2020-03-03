{- |
   Module      : Core.Types
   Description : All types needed for JSON parsing the openmensa API
   Copyright   : (c) Tony Zorman, 2019, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
{-# LANGUAGE DuplicateRecordFields #-}

module Core.Types
    ( -- * Types for 'Mensa' and its meals.
      Mensa(..)
    , Meals
    , Meal(..)
    , Prices(..)

    -- * Utility functions.
    , showMeals
    , empty
    , mkEmptyMensa
    ) where

-- Text
import           Data.Text ( Text )
import qualified Data.Text as T

-- Other imports
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.List (intersperse)
import GHC.Generics (Generic)


-- | 'Mensa' type
data Mensa = Mensa
    { name  :: Text
    , url   :: Text
    , meals :: Meals
    }

-- | A 'Mensa' is empty if it doensn't have any food to serve.
empty :: Mensa -> Bool
empty (Mensa _ _ []) = True
empty _              = False

-- | Helper function for creating an empty 'Mensa'
mkEmptyMensa :: Text -> Text -> Mensa
mkEmptyMensa n u = Mensa n u []

-- | A canteen serves food!
type Meals = [Meal]

-- | Type for a meal.
-- PONDER: It might be better to parse this as a generic JSON and then just
-- slurp out the fields that are interesting to us.
data Meal = Meal
    { id       :: Int
    , name     :: !Text
    , notes    :: ![Text]
    , prices   :: !Prices
    , category :: !Text
    , image    :: Text   -- ^ Sadly not an image of the actual food :(
    , url      :: Text
    } deriving (Generic, FromJSON)

-- | All the different price types.
data Prices
    = Prices { students  :: {-# UNPACK #-} !Double
             , employees :: Double
             }
    | NoPrice [Double]
    -- ^ Who at the Studentenwerk thought that this was a good idea?
    -- This will always be an empty list.

-- | Manually derive 'FromJSON' instance due to dumb field names.
instance FromJSON Prices where
    parseJSON (Object v) = Prices
        <$> (v .: "Studierende" <|> v .: "Preis 1")
        <*> (v .: "Bedienstete" <|> v .: "Preis 2")
    parseJSON _ = pure $ NoPrice []

-- | Pretty print only the things I'm interested in.
-- Yay, using 'Text' instead of 'String' \o/
showMeals
    :: Int    -- ^ Line wrap.
    -> Meals
    -> Text
showMeals _  [] = ""  -- Init is not safe on an empty list.
showMeals lw ms = T.init . T.unlines . map (showMeal lw) $ ms
  where
    -- | Pretty printing for a single 'Meal'.
    showMeal
        :: Int   -- ^ Line wrap.
        -> Meal
        -> Text
    showMeal wrap Meal{ category, name, notes, prices } =
           "\n" <> style nameText      <> wrapName wrap name
        <> "\n" <> style "Preis: "     <> tshowEUR (mstudents prices)
        <> "\n" <> style notesText     <> decodeSymbols (wrapNotes wrap notes)
        <> "\n" <> style "Kategorie: " <> category
      where
        nameText, notesText :: Text
        nameText  = "Essen: "
        notesText = "Notes: "

        -- | Wrapping for the menu name.
        wrapName :: Int -> Text -> Text
        wrapName w n = wrapWith " " (T.length nameText) w (T.words n)

        -- | Wrapping for the notes.
        wrapNotes :: Int -> [Text] -> Text
        wrapNotes = wrapWith ", " (T.length notesText)

        -- | Pretty printing for prices.
        tshowEUR :: Show a => a -> Text
        tshowEUR s = T.pack (show s) <> "€"

        {- | We're (as of now) only interested in the student prices.
           Anything with 'NoPrice' will be filtered out later, so it's value
           here is meaningless.
        -}
        mstudents :: Prices -> Double
        mstudents (Prices  s _) = s
        mstudents (NoPrice _  ) = -1

        -- | For some reason this is only needed on notes.
        decodeSymbols :: Text -> Text
        decodeSymbols = T.replace "&uuml;" "ü"
                      . T.replace "&lpar;" "("
                      . T.replace "&rpar;" ")"


        {- | Set the style for some keywords
           33 looks nice
           1 is bold
           see https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
        -}
        style :: Text -> Text
        style s = "\x1b[33m" <> s <> "\x1b[0m"
{-# INLINE showMeals #-}

-- | Simple (and probably hilariously inefficient) function to wrap text
-- at N columns.
wrapWith
    :: Text    -- ^ How to concatenate chunks
    -> Int     -- ^ Alignment
    -> Int     -- ^ Max line length (wrap)
    -> [Text]  -- ^ Text as chunks that have to stay together.
    -> Text    -- ^ Text with line breaks.
wrapWith divText al wrapAt chunks
    | wrapAt == 0 = mconcat $ intersperse divText chunks
    | otherwise   = go "" al chunks
  where
    go :: Text    -- ^ Text with line breaks.
       -> Int     -- ^ Counter of the current line length.
       -> [Text]  -- ^ Text as chunks that have to stay together.
       -> Text
    go l _ [] = l
    go line acc xs@(c:cs)
        | combLen >= wrapAt = go (align line)       al     xs
        | otherwise         = go (line <> c <> end) newLen cs
      where
        combLen = acc + T.length c
        newLen  = combLen + T.length end
        align   = (<> "\n" <> T.replicate al " ")
        end | null cs   = ""
            | otherwise = divText
{-# INLINE wrapWith #-}
