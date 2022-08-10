{- |
   Module      : Meal.Options
   Description : Various functions pertaining filtering meals.
   Copyright   : (c) Tony Zorman  2020 2021
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Meal.Options (
  -- * Options for filtering
  MealOptions (..),
  MealType (..),
  MealTime (..),
  Ignored (..),

  -- * Filter for the given options
  filterOptions, -- :: Options -> Meals -> Meals
) where

import Meal

import Data.Text qualified as T


-- | Options for filtering meals.
data MealOptions = MealOptions
  { mealType :: MealType
  , mealTime :: MealTime
  , iKat     :: [Text]    -- ^ *Ignored* categories
  , iNotes   :: [Text]    -- ^ *Ignored* notes
  , ignored  :: [Ignored] -- ^ Ignored stuff
  }

-- | Stuff one can ignore; i.e., meals the user might not want printed.
data Ignored
  = INotes [Text] -- ^ Ignored notes
  | ICat   [Text] -- ^ Ignored categories
  | IName  [Text] -- ^ Ignored names

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

-- | Filter for the meal options given; ignore anything that's already
-- sold out.
filterOptions :: MealOptions -> Meals -> Meals
filterOptions opts = filter availableOpts
 where
  -- Every predicate should be satisfied in order for the result to be
  -- accepted.
  availableOpts :: Meal -> Bool
  availableOpts meal = all ($ meal) (getAllOpts opts)

-- | All of the options a user picked.
getAllOpts :: MealOptions -> [Meal -> Bool]
getAllOpts MealOptions{ mealType, mealTime, iKat, iNotes, ignored } =
  concat
    [ [notSoldOut, fitsDiet, correctTimeOfDay]
    , foldMap' ignoreThing ignored
    , map notCategory    iKat
    , map notPartOfNotes iNotes
    ]
 where
  fitsDiet :: Meal -> Bool
  fitsDiet = case mealType of
    Vegetarian -> eitherOf vegan vegetarian
    Vegan      -> vegan
    AllMeals   -> const True

  correctTimeOfDay :: Meal -> Bool
  correctTimeOfDay = case mealTime of
    AllDay -> const True
    Dinner -> dinner
    Lunch  -> not . dinner

  ignoreThing :: Ignored -> [Meal -> Bool]
  ignoreThing = \case
    INotes s -> map notPartOfNotes s
    ICat   s -> map notCategory    s
    IName  s -> map notName        s

  notName, notCategory, notPartOfNotes :: Text -> Meal -> Bool
  notName        s = not . (s `T.isInfixOf`)     . name
  notCategory    s =       (s /=)                . category
  notPartOfNotes s = not . any (s `T.isInfixOf`) . notes

  -- See if meal is vegetarian or there's some sort of vegetarian
  -- variant available.
  vegetarian :: Meal -> Bool
  vegetarian = eitherOf (inNotes "Menü ist vegetarisch") (inName "vegetarisch")

  -- See if meal is vegan or there's some vegetarian variant available.
  vegan :: Meal -> Bool
  vegan = eitherOf (inNotes "Menü ist vegan") (inName "vegan")

  dinner :: Meal -> Bool
  dinner = ("Abendangebot" `T.isInfixOf`) . category

  notSoldOut :: Meal -> Bool
  notSoldOut = prices >>> \case
    SoldOut -> False
    _       -> True

  inNotes :: Text -> Meal -> Bool
  inNotes s = (s `elem`) . notes

  inName :: Text -> Meal -> Bool
  inName s = (s `T.isInfixOf`) . name
