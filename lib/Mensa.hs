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
  ppMensen,          -- :: DatePP -> MensaOptions [Mensa] -> [Text]
) where

import Meal
import Prelude hiding (Prefix)
import Time

import Data.List qualified as List
import Data.Text qualified as T


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
  { canteen   :: mensa
  , sections  :: [Section] -- ^ Sections to be printed
  , noAdds    :: Bool      -- ^ Whether to show the additive notes in
                           --   parentheses, like @(A, A1, C, G)@
  , lineWrap  :: Natural   -- ^ Line wrap in the output
  , doubleCol :: Bool      -- ^ Print the meals in a double column layout?
  }

-- | A possible prefix that will be style with ANSI escape codes.
data Prefix = Prefix Text | NoPrefix

-- | Pretty print multiple canteens.
ppMensen :: DatePP -> MensaOptions [Mensa] -> [Text]
ppMensen date opts@MensaOptions{ lineWrap, doubleCol, canteen = canteens }
  = toDoubleColumn lineWrap doubleCol
  . map (\m -> ppMensa date opts{ canteen = m })
  . filter (not . null . meals)
  $ canteens

-- | Pretty print a single canteen.
ppMensa :: DatePP -> MensaOptions Mensa -> [[[Text]]]
ppMensa day opts@MensaOptions{ lineWrap, canteen = Mensa{ name, meals } }
  | null meals = []  -- Don't show empty canteens
  | otherwise  = [[sep, fill NoPrefix lineWrap (day <> " in: " <> name), sep]]
               : ppMeals opts
 where
  -- Separator for visual separation of different canteens.
  sep :: Text
  sep = T.replicate (if lineWrap > 0 then fi lineWrap else 79) "="

-- | Pretty print only the things I'm interested in.
ppMeals :: MensaOptions Mensa -> [[[Text]]]
ppMeals MensaOptions{ lineWrap, noAdds, canteen, sections }
  = map (\meal -> map (ppSection meal) sections)
        (meals canteen)
 where
  -- Pretty print a single section of a 'Meal'.  If the associated
  -- flavour text is empty then ignore the section.
  ppSection :: Meal -> Section -> [Text]
  ppSection Meal{ category, name, notes, prices } section
    | null flavourText = []
    | otherwise        = addFirst (tshow section) flavourText
   where
    flavourText :: [Text]
    flavourText = case section of
      Name     -> wrapName
      Price    -> wrapPrices
      Notes    -> map decodeSymbols wrapNotes
      Category -> [category]

    wrapName :: [Text]
    wrapName = wrapSec " " Name (words $ ignoreAdditives name)

    wrapNotes :: [Text]
    wrapNotes = wrapSec ", " Notes (map ignoreAdditives notes)

    wrapPrices :: [Text]
    wrapPrices = case prices of
      SoldOut    -> []           -- will be filtered out later
      Prices s e -> wrapSec ", " Price [ "Studierende: " <> tshow s <> "€"
                                       , "Bedienstete: " <> tshow e <> "€"
                                       ]

    wrapSec :: Text -> Section -> [Text] -> [Text]
    wrapSec s sec = wrapWith s (length $ tshow sec) (fi lineWrap)

    -- For some reason only the notes are not escaped properly.
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

    addFirst :: Text -> [Text] -> [Text]
    addFirst _ []       = []
    addFirst s (x : xs) = fill (Prefix s) lineWrap x
                        : map (fill NoPrefix lineWrap) xs

-- | Fill a bunch of text, in order to make aligning easier.
fill :: Prefix -> Natural -> Text -> Text
fill pfx (fi -> lw) str =
  style prefix <> str <> if len < lw then T.replicate (lw - len) " " else ""
 where
  len    = length str + length prefix
  prefix = case pfx of
    Prefix p -> p
    NoPrefix -> ""

  -- See https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
  style :: Text -> Text
  style s = "\x1b[33m" <> s <> "\x1b[0m"

toDoubleColumn :: Natural -> Bool -> [[[[Text]]]] -> [Text]
toDoubleColumn (fi -> lw) dc ms
  = map (T.unlines . map (mconcat . map T.unlines)) go
 where
  go :: [[[[Text]]]]  -- lol
  go | dc = zipWith (\men men' ->                             -- mensa
              zipWith (\meal meal' ->                         -- meal
                zipWith (\sec sec' ->                         -- section
                  zipWith (\l l' -> l <> "    " <> l')        -- line
                          `uncurry` mkEven (T.replicate lw " ") sec sec')
                        `uncurry` mkEven mempty meal meal')
                      `uncurry` mkEven mempty men men')
                    `uncurry` (mkEven [] `uncurry` chunks ms)
     | otherwise = ms

  mkEven :: a -> [a] -> [a] -> ([a], [a])
  mkEven filler a b
    | List.length a < List.length b = (a <> repeat filler, b)
    | otherwise                     = (a, b <> repeat filler)

  chunks :: [a] -> ([a], [a])
  chunks = work ([], [])
   where
    work :: ([a], [a]) -> [a] -> ([a], [a])
    work res      []       = res
    work (as, bs) [x]      = (x : as, bs)
    work (as, bs) (x:y:xs) = work (x : as, y : bs) xs
