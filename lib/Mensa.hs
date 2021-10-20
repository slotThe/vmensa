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
  Mensa (..),
  MensaOptions (..),

  -- * Pretty printing
  Section (..),      -- instances: Eq, Show
  ppMensa,           -- :: Natural -> [Section] -> Text -> Bool -> Mensa -> Text
) where

import Meal
import Time

import qualified Data.Text as T


-- | 'Mensa' type, all fields are needed and hence all fields are
-- strict.
data Mensa = Mensa
  { name  :: Text
  , url   :: Text
  , meals :: Meals
  }

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

-- | Pretty-printing options for a canteen.
data MensaOptions mensa = MensaOptions
  { canteen  :: mensa
  , sections :: [Section] -- ^ Sections to be printed
  , noAdds   :: Bool      -- ^ Whether to show the additive notes in
                          --   parentheses, like @(A, A1, C, G)@
  , lineWrap :: Natural   -- ^ Line wrap in the output
  }

-- | Pretty print a single canteen.
ppMensa :: DatePP -> MensaOptions Mensa -> Text
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

    -- See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
    style :: Text -> Text
    style s = "\x1b[33m" <> s <> "\x1b[0m"

    wrapName :: Text
    wrapName = wrapSec " " Name (words $ ignoreAdditives name)

    wrapNotes :: Text
    wrapNotes = wrapSec ", " Notes (map ignoreAdditives notes)

    wrapPrices :: Text
    wrapPrices = case prices of
      SoldOut    -> ""           -- will be filtered out later
      Prices s e -> wrapSec ", " Price [ "Studierende: " <> tshow s <> "€"
                                       , "Bedienstete: " <> tshow e <> "€"
                                       ]

    wrapSec :: Text -> Section -> [Text] -> Text
    wrapSec s sec xs = wrapWith s (length $ tshow sec) (fi lineWrap) xs

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
