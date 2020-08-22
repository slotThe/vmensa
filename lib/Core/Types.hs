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
    , Meal(..)      -- instances: Generic, FromJSON
    , Meals         -- types alias: [Meal]
    , Prices(..)    -- instances: FromJSON
    , MealType(..)
    , MealTime(..)

    -- * Pretty printing
    , Section(..)   -- instances: Eq, Show
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

-- | Construct an empty (i.e. no food to serve) 'Mensa'.
mkEmptyMensa :: (Text, Text -> Text) -> Text -> Mensa
mkEmptyMensa (n, urlWithoutDate) d = Mensa n (urlWithoutDate d) []

-- | Type for a single meal.  Note that we are only specifying the contents of
-- the JSON that we will actually use.
data Meal = Meal
    { name     :: !Text
    , notes    :: ![Text]
    , prices   :: !Prices
    , category :: !Text
    } deriving (Generic, FromJSON)

-- | A canteen serves food!
type Meals = [Meal]

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

-- | Section of the JSON output we would like to print.
data Section
    = Name
    | Notes
    | Price
    | Category
    deriving (Eq)

instance Show Section where
    show :: Section -> String
    show = \case
        Name     -> "Essen: "
        Notes    -> "Notes: "
        Price    -> "Preis: "
        Category -> "Kategorie: "

-- | Pretty print a 'Mensa'.
ppMensa
    :: Int        -- ^ Line wrap
    -> [Section]  -- ^ Sections to be displayed
    -> Text       -- ^ Day when the meals are offered
    -> Mensa
    -> Text
ppMensa lw sections day = \case
    Mensa _ _ []         -> ""  -- Don't show empty canteens
    Mensa{ name, meals } -> T.unlines [sep, day <> " in: " <> name, sep]
                         <> ppMeals lw sections meals
  where
    -- | Separator for visual separation of different canteens.
    sep :: Text
    sep = T.replicate (if lw > 0 then lw else 79) "="

-- | Pretty print only the things I'm interested in.
ppMeals :: Int -> [Section] -> Meals -> Text
ppMeals lw sections meals =
    T.unlines $ map (\meal -> mconcat $ map (ppSection meal) sections) meals
  where
    -- | Pretty print a single section of a 'Meal'.  If the associated flavour
    -- text is empty ignore the section.
    ppSection :: Meal -> Section -> Text
    ppSection Meal{ category, name, notes, prices } section
        | T.null flavourText = ""
        | otherwise          = style (tshow section) <> flavourText <> "\n"
      where
        flavourText :: Text
        flavourText = case section of
            Name     -> wrapName
            Price    -> tshow studentPrice <> "€"
            Notes    -> decodeSymbols wrapNotes
            Category -> category

        -- | See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
        style :: Text -> Text
        style s = "\x1b[33m" <> s <> "\x1b[0m"

        wrapName :: Text
        wrapName = wrapWith " " (T.length $ tshow Name) lw (T.words name)

        wrapNotes :: Text
        wrapNotes = wrapWith ", " (T.length $ tshow Notes) lw notes

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

-- | Simple (and probably hilariously inefficient) function to wrap text at @N@
-- columns.
wrapWith
    :: Text    -- ^ How to concatenate chunks, i.e. the separator
    -> Int     -- ^ Left alignment
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

        -- | Nicely left-align the text after a line-break.  We like pretty things.
        align :: Text -> Text
        align = (<> "\n" <> T.replicate al " ")

        end :: Text
        end = if null cs then "" else sep
