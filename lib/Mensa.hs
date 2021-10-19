{- |
   Module      : Mensa
   Description : The representation for a generic canteen
   Copyright   : (c) Tony Zorman  2019 2020 2021
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Mensa (
  -- * Types for 'Mensa' and its meals.
  PreMensa,          -- abstract
  Mensa (..),
  MensaOptions (..),
  Options (..),

  -- * Constructing canteens
  mkEmptyMensa,      -- :: (Text, Text -> Text) -> PreMensa
  mkMensa,           -- :: Text -> PreMensa -> Mensa

  -- * Pretty printing
  Section (..),      -- instances: Eq, Show
  ppMensa,           -- :: Natural -> [Section] -> Text -> Bool -> Mensa -> Text
) where

import Meal
import Meal.Options

import qualified Data.Text as T


-- | A 'PreMensa' is just a 'Mensa' that's still waiting for a date;
-- this date will be used to generate the correct URL.
newtype PreMensa = PreMensa { unPM :: Text -> Mensa }

-- | 'Mensa' type, all fields are needed and hence all fields are
-- strict.
data Mensa = Mensa
  { name  :: Text
  , url   :: Text
  , meals :: Meals
  }

-- | Construct an empty (i.e. no food to serve) 'Mensa'.
mkEmptyMensa :: (Text, Text -> Text) -> PreMensa
mkEmptyMensa (name, urlNoDate) = PreMensa \d -> Mensa name (urlNoDate d) []

-- | Create a 'Mensa' out of a 'PreMensa', as well as a date.
mkMensa :: Text -> PreMensa -> Mensa
mkMensa = flip unPM

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

-- | Options with which to pretty-print a canteen.  These will double as
-- command line options.
data Options date = Options
  { mealOptions  :: MealOptions
  , mensaOptions :: MensaOptions [PreMensa]
  , date         :: date  -- ^ Access date
  }

data MensaOptions mensa = MensaOptions
  { canteen :: mensa
  , sections :: [Section] -- ^ Sections to be printed
  , noAdds   :: Bool      -- ^ Whether to show the additive notes in
                          --   parentheses, like @(A, A1, C, G)@
  , lineWrap :: Natural   -- ^ Line wrap in the output
  }

-- | Pretty print a single canteen.
ppMensa :: Text -> MensaOptions Mensa -> Text
ppMensa day opts@MensaOptions{ lineWrap, canteen = Mensa{ name, meals } }
  | null meals = ""  -- Don't show empty canteens
  | otherwise  = T.unlines [sep, day <> " in: " <> name, sep]
              <> ppMeals opts
 where
  -- | Separator for visual separation of different canteens.
  sep :: Text
  sep = T.replicate (if lineWrap > 0 then fi lineWrap else 79) "="

-- | Pretty print only the things I'm interested in.
ppMeals :: MensaOptions Mensa -> Text
ppMeals MensaOptions{ lineWrap, noAdds, canteen, sections }
  = T.unlines $ map (\meal -> foldMap' (ppSection meal) sections)
                    (meals canteen)
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
      Price    -> wrapPrices
      Notes    -> decodeSymbols wrapNotes
      Category -> category

    -- | See https:\/\/en.wikipedia.org\/wiki\/ANSI_escape_code#SGR_parameters
    style :: Text -> Text
    style s = "\x1b[33m" <> s <> "\x1b[0m"

    wrapName :: Text
    wrapName = wrapWith' " " Name (words $ ignoreAdditives name)

    wrapNotes :: Text
    wrapNotes = wrapWith' ", " Notes (map ignoreAdditives notes)

    -- | Anything with 'SoldOut' will be filtered out later, so its
    -- value here is meaningless.
    wrapPrices :: Text
    wrapPrices = case prices of
      SoldOut    -> ""
      Prices s e -> wrapWith' ", " Price [ "Studierende: " <> tshow s <> "€"
                                         , "Bedienstete: " <> tshow e <> "€"
                                         ]

    wrapWith' :: Text -> Section -> [Text] -> Text
    wrapWith' s sec xs = wrapWith s (length $ tshow sec) (fi lineWrap) xs

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
