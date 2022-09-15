{- |
   Module      : Mensa
   Description : The representation for a generic canteen
   Copyright   : (c) Tony Zorman  2019 2020 2021 2022
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Mensa (
  -- * Types for 'Mensa' and its meals.
  Mensa (..),
  MensaState (..),
  MensaOptions (..),

  -- * Combinators
  addDate,           -- :: Text -> Mensa 'Incomplete -> Mensa 'NoMeals
  addMeals,          -- :: Meals -> Mensa 'NoMeals -> Mensa 'Complete
  mkIncompleteMensa, -- :: Text -> (Text -> Text) -> Mensa 'Incomplete
  url,               -- :: Mensa a -> Maybe Text

  -- * Pretty printing
  Section (..),      -- instances: Eq, Show
  ppMensen,          -- :: Text -> MensaOptions [Mensa 'Complete] -> [Text]
) where

import Meal
import Util hiding (Prefix)

import CmdLine.Util (wrapText)
import Data.Kind    (Type)
import Data.List    qualified as List
import Data.Text    qualified as T

-- | 'Mensa' type, all fields are needed and hence all fields are
-- strict.
type Mensa :: MensaState -> Type
data Mensa (m :: MensaState) where
  IncompleteMensa :: MensaData 'Incomplete -> Mensa 'Incomplete
  NoMealsMensa    :: MensaData 'NoMeals    -> Mensa 'NoMeals
  CompleteMensa   :: MensaData 'Complete   -> Mensa 'Complete

type MensaData :: MensaState -> Type
data MensaData (state :: MensaState) where
  IncompleteData :: Text -> (Text -> Text) -> ()    -> MensaData 'Incomplete
  NoMealsData    :: Text -> Text           -> ()    -> MensaData 'NoMeals
  CompleteData   :: Text -> Text           -> Meals -> MensaData 'Complete

addDate :: Text -> Mensa 'Incomplete -> Mensa 'NoMeals
addDate date (IncompleteMensa (IncompleteData n f ())) =
  NoMealsMensa (NoMealsData n (f date) ())

addMeals :: Meals -> Mensa 'NoMeals -> Mensa 'Complete
addMeals ms (NoMealsMensa (NoMealsData n u ())) =
  CompleteMensa (CompleteData n u ms)

mkIncompleteMensa :: Text -> (Text -> Text) -> Mensa 'Incomplete
mkIncompleteMensa name urlNoDate =
  IncompleteMensa (IncompleteData name urlNoDate ())

meals :: Mensa 'Complete -> Meals
meals (CompleteMensa (CompleteData _ _ m)) = m

url :: Mensa a -> Maybe Text
url = \case
  IncompleteMensa IncompleteData{}     -> Nothing
  NoMealsMensa    (NoMealsData  _ u _) -> Just u
  CompleteMensa   (CompleteData _ u _) -> Just u

mensaName :: Mensa a -> Text
mensaName = \case
  IncompleteMensa (IncompleteData n _ _) -> n
  NoMealsMensa    (NoMealsData    n _ _) -> n
  CompleteMensa   (CompleteData   n _ _) -> n

data MensaState = Complete | Incomplete | NoMeals

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
  , columns   :: Natural   -- ^ Print the meals in an n-column layout
  }

-- | A possible prefix that will be style with ANSI escape codes.
data Prefix = Prefix Text | NoPrefix

-- | Pretty print multiple canteens.
ppMensen :: Text -> MensaOptions [Mensa 'Complete] -> [Text]
ppMensen day opts@MensaOptions{ lineWrap = lw, columns, canteen = canteens }
  = toColumns lw columns
  . map (\m -> ppMensa opts{ canteen = m })
  . filter (not . null . meals)
  $ canteens
 where
  -- Pretty print a single canteen.
  ppMensa :: MensaOptions (Mensa 'Complete) -> [[[Text]]]
  ppMensa mopts@(MensaOptions{ canteen }) =
    let fullName  = day <> " in: " <> mensaName canteen
        shortName = if   fi lw < length fullName
                    then T.take (fi lw - 2) fullName <> "…"
                    else fullName
     in [[sep, fill NoPrefix lw shortName, sep]] : ppMeals mopts

  -- Separator for visual separation of different canteens.
  sep :: Text
  sep = T.replicate (if lw > 0 then fi lw else 79) "="

-- | Pretty print only the things I'm interested in.
ppMeals :: MensaOptions (Mensa 'Complete) -> [[[Text]]]
ppMeals (MensaOptions{ lineWrap, noAdds, sections, canteen })
  = map (\meal -> map (ppSection meal) sections) (meals canteen)
 where
  -- Pretty print a single section of a 'Meal'.  If the associated
  -- flavour text is empty then ignore the section.
  ppSection :: Meal -> Section -> [Text]
  ppSection Meal{ category, name, notes, prices } section
    | T.null flavourText = []
    | otherwise          = styleFirst (tshow section) (T.lines flavourText)
   where
    flavourText :: Text
    flavourText = case section of
      Name     -> wrapName
      Price    -> wrapPrices
      Notes    -> decodeSymbols wrapNotes
      Category -> category

    wrapName, wrapNotes, wrapPrices :: Text
    wrapName   = wrapSec " "  Name  (words $ ignoreAdditives name)
    wrapNotes  = wrapSec ", " Notes (map ignoreAdditives notes)
    wrapPrices = case prices of
      SoldOut    -> ""           -- will be filtered out later
      Prices s e -> wrapSec ", " Price [ "Studierende: " <> tshow s <> "€"
                                       , "Bedienstete: " <> tshow e <> "€"
                                       ]

    wrapSec :: Text -> Section -> [Text] -> Text
    wrapSec s sec = wrapText s (length $ tshow sec) (fi lineWrap)

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

    styleFirst :: Text -> [Text] -> [Text]
    styleFirst _ []       = []
    styleFirst s (x : xs) = fill (Prefix s) lineWrap x
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

toColumns :: Natural -> Natural -> [[[[Text]]]] -> [Text]
toColumns (fi -> lw) (fi -> cols) ms
  = map (T.unlines . map (mconcat . map T.unlines)) go
 where
  go :: [[[[Text]]]]  -- lol
  go | cols <= 1 = ms
     | otherwise =
         map                                                -- canteens
           (map                                             -- meals
              (map                                          -- sections
                 (map (T.intercalate "    ")                -- lines
                      . intertwine (T.replicate lw " "))
                 . intertwine mempty)
              . intertwine mempty)
           (mkEven [] (chunks ms))

  chunks :: [a] -> [[a]]
  chunks = takeWhile (not . null) . unfoldr (Just . splitAt cols)

  intertwine :: a -> [[a]] -> [[a]]
  intertwine def (mkEven def -> xs) = map (\i -> map (!! i) xs) [0 .. (n - 1)]
   where n = List.length (head xs)

  mkEven :: a -> [[a]] -> [[a]]
  mkEven def xs = map (\lst -> lst <> replicate (n - List.length lst) def) xs
   where n = maximum (map List.length xs)
