{- |
   Module      : Core.Types
   Description : Basic types a canteen might need
   Copyright   : (c) Tony Zorman  2019 2020 2021
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.Types (
    -- * Types for 'Mensa' and its meals.
    PreMensa,       -- abstract
    Mensa (..),
    Meal (..),      -- instances: Generic, FromJSON
    Meals,          -- types alias: [Meal]
    Prices (..),    -- instances: FromJSON
    MealType (..),
    MealTime (..),

    -- * Pretty printing
    Section (..),   -- instances: Eq, Show
    ppMensa,        -- :: Natural -> [Section] -> Text -> Bool -> Mensa -> Text

    -- * Constructing canteens
    mkEmptyMensa,   -- :: Text -> (Text, Text -> Text) -> Mensa
    mkMensa,        -- :: Text -> PreMensa -> Mensa
) where

import qualified Data.Text as T

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
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

-- | A 'PreMensa' is just a 'Mensa' that's still waiting for a date;
-- this date will be used to generate the correct URL.
newtype PreMensa = PreMensa { unPM :: Text -> Mensa }

-- | 'Mensa' type, all fields are needed and hence all fields are
-- strict.
data Mensa = Mensa
    { name  :: !Text
    , url   :: !Text
    , meals :: !Meals
    }

-- | Construct an empty (i.e. no food to serve) 'Mensa'.
mkEmptyMensa :: (Text, Text -> Text) -> PreMensa
mkEmptyMensa (name, urlNoDate) = PreMensa \d -> Mensa name (urlNoDate d) []

-- | Create a 'Mensa' out of a 'PreMensa', as well as a date.
mkMensa :: Text -> PreMensa -> Mensa
mkMensa = flip unPM

-- | Type for a single meal.  Note that we are only specifying the
-- contents of the JSON that we will actually use.
data Meal = Meal
    { name     :: !Text
    , notes    :: ![Text]
    , prices   :: !Prices
    , category :: !Text
    }
    deriving stock    (Generic)
    deriving anyclass (FromJSON)

-- | A canteen serves food!
type Meals = [Meal]

-- | All the different price types.  Note again that we are only
-- specifying the contents of the JSON that we will actually use.
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
    deriving stock (Eq)

instance Show Section where
    show :: Section -> String
    show = \case
        Name     -> "Essen: "
        Notes    -> "Notes: "
        Price    -> "Preis: "
        Category -> "Kategorie: "

-- | Pretty print a single canteen.
ppMensa
    :: Natural    -- ^ Line wrap
    -> [Section]  -- ^ Sections to be displayed
    -> Text       -- ^ Day when the meals are offered
    -> Bool       -- ^ Whether to display letters for additives
    -> Mensa
    -> Text
ppMensa lw sections day noAdds Mensa{ name, meals }
    | null meals = ""  -- Don't show empty canteens
    | otherwise  = T.unlines [sep, day <> " in: " <> name, sep]
                <> ppMeals lw sections noAdds meals
  where
    -- | Separator for visual separation of different canteens.
    sep :: Text
    sep = T.replicate (if lw > 0 then fi lw else 79) "="

-- | Pretty print only the things I'm interested in.
ppMeals :: Natural -> [Section] -> Bool -> Meals -> Text
ppMeals lw sections noAdds meals =
    T.unlines $ map (\meal -> foldMap' (ppSection meal) sections) meals
  where
    -- | Pretty print a single section of a 'Meal'.  If the associated
    -- flavour text is empty then ignore the section.
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

        -- | See https:\/\/en.wikipedia.org\/wiki\/ANSI_escape_code#SGR_parameters
        style :: Text -> Text
        style s = "\x1b[33m" <> s <> "\x1b[0m"

        wrapName :: Text
        wrapName = wrapWith " " (length $ tshow Name) (fi lw) (words $ ignoreAdditives name)

        wrapNotes :: Text
        wrapNotes = wrapWith ", " (length $ tshow Notes) (fi lw) (map ignoreAdditives notes)

        {- | We're (as of now) only interested in the student prices.
           Anything with 'SoldOut' will be filtered out later, so it's
           value here is meaningless.
        -}
        studentPrice :: Double
        studentPrice = case prices of
            Prices s -> s
            SoldOut  -> -1

        -- | For some reason only the notes are not escaped properly.
        decodeSymbols :: Text -> Text
        decodeSymbols
            = replace "&uuml;" "ü"
            . replace "&auml;" "ä"
            . replace "&ouml;" "ö"
            . replace "&lpar;" "("
            . replace "&rpar;" ")"
            . replace "&excl;" "!"

        ignoreAdditives :: Text -> Text
        ignoreAdditives = if noAdds then go else id
          where
            go str | T.null str = ""
                   | otherwise  = str' <> go rest
              where
                (str', rest) = bimap T.stripEnd (T.drop 1 . T.dropWhile (/= ')'))
                             $ T.breakOn "(" str

{- | Simple (and probably hilariously inefficient) function to wrap text
at @N@ columns.

NOTE: "Data.Text"s 'Data.Text.length' function is @O(n)@, which may or
      may not matter here.
-}
wrapWith
    :: Text    -- ^ How to concatenate chunks, i.e. the separator
    -> Int     -- ^ Left alignment
    -> Int     -- ^ Max line length (wrap)
    -> [Text]  -- ^ Text as chunks that have to stay together
    -> Text    -- ^ Text with line breaks
wrapWith separator al wrapAt chunks
    | wrapAt == 0 = mconcat $ intersperse separator chunks
    | otherwise   = go "" separator al chunks
  where
    go :: Text    -- ^ Already processed part of the text
       -> Text    -- ^ Separator to put between chunks
       -> Int     -- ^ Counter of the current line length
       -> [Text]  -- ^ Text as chunks that have to stay together
       -> Text
    go !done _   !_   []        = done
    go !line sep !acc xs@(c:cs)
        | cLen      >= wrapAt = go goAgain            sep newLen cs
        | al + cLen >= wrapAt = go (goAgain <> ", ")  sep newLen cs
        | combLen   >= wrapAt = go (align line)       sep al     xs
        | otherwise           = go (line <> c <> end) sep newLen cs
      where
        goAgain :: Text = go line " " acc (words c)
        cLen    :: Int  = length c
        combLen :: Int  = acc + cLen            -- Length including the next word
        newLen  :: Int  = combLen + length end  -- Take separator length into account

        -- | Nicely left-align the text after a line-break.  We like
        -- pretty things.
        align :: Text -> Text
        align = (<> "\n" <> T.replicate al " ")

        end :: Text
        end = if null cs then "" else sep
