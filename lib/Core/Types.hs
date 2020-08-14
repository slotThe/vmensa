{- |
   Module      : Core.Types
   Description : Basic types a canteen might need
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
    , MealType(..)
    , MealTime(..)

    -- * Pretty printing
    , ppMensa       -- :: Int -> Text -> Mensa -> Text

    -- * Constructing canteens
    , mkEmptyMensa  -- :: Text -> (Text, Text -> Text) -> Mensa
    ) where

import qualified Data.Text as T

import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:))
import Data.Aeson.Types (Parser)


-- | What type of meal are we looking for?
data MealType
    = AllMeals
    | Vegetarian
    | Vegan

-- | Which time of day should the meal happen at?
data MealTime
    = Dinner
    | Lunch
    | AllDay

-- | 'Mensa' type, all fields are needed and hence all fields are strict.
data Mensa = Mensa
    { name  :: !Text
    , url   :: !Text
    , meals :: !Meals
    }

mkEmptyMensa :: Text -> (Text, Text -> Text) -> Mensa
mkEmptyMensa d (n, urlWithoutDate) = Mensa n (urlWithoutDate d) []

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
    | SoldOut

-- | Manually derive 'FromJSON' instance due to dumb field names.
instance FromJSON Prices where
    parseJSON :: Value -> Parser Prices
    parseJSON = \case
        Object v -> Prices <$> (v .: "Studierende" <|> v .: "Preis 1")
        _        -> pure SoldOut

-- | Pretty print a 'Mensa'.
ppMensa
    :: Int    -- ^ Line wrap
    -> Text   -- ^ Day when the meals are offered
    -> Mensa
    -> Text
ppMensa lw d mensa@Mensa{ name, meals }
    | empty mensa = ""
    | otherwise   = T.unlines [separator, d <> " in: " <> name, separator]
                 <> showMeals lw meals
  where
    -- | A 'Mensa' is empty if it doensn't have any food to serve.
    empty :: Mensa -> Bool
    empty (Mensa _ _ xs) = null xs

    -- | Separator for visual separation of different canteens.
    separator :: Text
    separator = T.pack $ replicate (if lw <= 0 then 80 else lw) '='

-- | Pretty print only the things I'm interested in.
showMeals :: Int -> Meals -> Text
showMeals lw = T.unlines . map showMeal
  where
    -- | Pretty printing for a single 'Meal'.
    showMeal :: Meal -> Text
    showMeal Meal{ category, name, notes, prices } = mconcat $ map checkPP
        [ (nameText     , wrapName                 )
        , ("Preis: "    , tshow studentPrice <> "€")
        , (notesText    , decodeSymbols wrapNotes  )
        , ("Kategorie: ", category                 )
        ]
      where
        -- | If the flavour text is null ignore the category, otherwise
        -- pretty-print it.
        checkPP :: (Text, Text) -> Text
        checkPP (n, t) = if T.null t then "" else style n <> t <> "\n"
          where
            -- | See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
            style :: Text -> Text
            style s = "\x1b[33m" <> s <> "\x1b[0m"

        wrapName :: Text
        wrapName = wrapWith " " (T.length nameText) lw (T.words name)

        wrapNotes :: Text
        wrapNotes = wrapWith ", " (T.length notesText) lw notes

        nameText, notesText :: Text
        nameText  = "Essen: "
        notesText = "Notes: "

        {- | We're (as of now) only interested in the student prices.
           Anything with 'SoldOut' will be filtered out later, so it's value
           here is meaningless.
        -}
        studentPrice :: Double
        studentPrice = case prices of
            Prices s -> s
            SoldOut  -> -1

        -- | For some reason only the notes are not escaped properly.
        decodeSymbols :: Text -> Text
        decodeSymbols
            = T.replace "&uuml;" "ü"
            . T.replace "&auml;" "ä"
            . T.replace "&ouml;" "ö"
            . T.replace "&lpar;" "("
            . T.replace "&rpar;" ")"
            . T.replace "&excl;" "!"

-- | Simple (and probably hilariously inefficient) function to wrap text
-- at N columns.
wrapWith
    :: Text    -- ^ How to concatenate chunks, i.e. the separator
    -> Int     -- ^ Alignment
    -> Int     -- ^ Max line length (wrap)
    -> [Text]  -- ^ Text as chunks that have to stay together
    -> Text    -- ^ Text with line breaks
wrapWith sep al wrapAt chunks
    | wrapAt == 0 = mconcat $ intersperse sep chunks
    | otherwise   = go "" al chunks
  where
    go :: Text    -- ^ Already processed part of the text
       -> Int     -- ^ Counter of the current line length
       -> [Text]  -- ^ Text as chunks that have to stay together
       -> Text
    go !done _   []         = done
    go !line acc xs@(c:cs)
        | combLen >= wrapAt = go (align line)       al     xs
        | otherwise         = go (line <> c <> end) newLen cs
      where
        combLen, newLen :: Int
        combLen = acc + T.length c        -- Length including the next word
        newLen  = combLen + T.length end  -- Take separator length into account

        -- | Nicely align the text after a line-break.  We like pretty things.
        align :: Text -> Text
        align = (<> "\n" <> T.replicate al " ")

        end :: Text
        end = if null cs then "" else sep
