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
    ( -- * Types for 'Mensa' and its meals.
      Mensa(..)
    , Meals         -- types alias: [Meal]
    , Meal(..)      -- instances: Generic, FromJSON
    , Prices(..)    -- intsances: FromJSON

    -- * Utility functions.
    , showMeals     -- :: Int -> Meals -> Text
    , empty         -- :: Mensa -> Bool
    , mkEmptyMensa  -- :: Text -> Text -> Mensa
    ) where

import qualified Data.Text as T

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Aeson.Types (Parser)


-- | 'Mensa' type, all fields are needed and hence all fields are strict.
data Mensa = Mensa
    { name  :: !Text
    , url   :: !Text
    , meals :: !Meals
    }

-- | A 'Mensa' is empty if it doensn't have any food to serve.
empty :: Mensa -> Bool
empty = \case
    Mensa _ _ [] -> True
    _            -> False

-- | Helper function for creating an empty 'Mensa'
mkEmptyMensa :: Text -> Text -> Mensa
mkEmptyMensa n u = Mensa n u []

-- | A canteen serves food!
type Meals = [Meal]

-- | Type for a single meal.  Note that we are only specifying the contents of
-- the JSON that we will actually use.
data Meal = Meal
    { name     :: !Text
    , notes    :: ![Text]
    , prices   :: !Prices
    , category :: !Text
    } deriving (Generic, FromJSON)

-- | All the different price types.  Note again that we are only specifying the
-- contents of the JSON that we will actually use.
data Prices
    = Prices { student :: !Double }
    | NoPrice

-- | Manually derive 'FromJSON' instance due to dumb field names.
instance FromJSON Prices where
    parseJSON :: Value -> Parser Prices
    parseJSON = \case
        Object v -> Prices <$> (v .: "Studierende" <|> v .: "Preis 1")
        _        -> pure NoPrice

-- | Pretty print only the things I'm interested in.
showMeals
    :: Int    -- ^ Line wrap.
    -> Meals
    -> Text
showMeals lw = T.unlines . map showMeal
  where
    -- | Pretty printing for a single 'Meal'.
    showMeal :: Meal -> Text
    showMeal Meal{ category, name, notes, prices } = T.unlines
        [ style nameText      <> wrapName
        , style "Preis: "     <> showEUR studentPrice
        , style notesText     <> decodeSymbols wrapNotes
        , style "Kategorie: " <> category
        ]
      where
        nameText, notesText :: Text
        nameText  = "Essen: "
        notesText = "Notes: "

        wrapName :: Text
        wrapName = wrapWith " " (T.length nameText) lw (T.words name)

        wrapNotes :: Text
        wrapNotes = wrapWith ", " (T.length notesText) lw notes

        showEUR :: Show a => a -> Text
        showEUR = (<> "€") . tshow

        {- | We're (as of now) only interested in the student prices.
           Anything with 'NoPrice' will be filtered out later, so it's value
           here is meaningless.
        -}
        studentPrice :: Double
        studentPrice = case prices of
            Prices s -> s
            NoPrice  -> -1

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
    go !l    _    []        = l
    go !line !acc xs@(c:cs)
        | combLen >= wrapAt = go (align line)       al     xs
        | otherwise         = go (line <> c <> end) newLen cs
      where
        combLen, newLen :: Int
        combLen = acc + T.length c
        newLen  = combLen + T.length end

        align :: Text -> Text
        align = (<> "\n" <> T.replicate al " ")

        end :: Text
        end = if null cs then "" else divText
