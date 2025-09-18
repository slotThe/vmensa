{- |
   Module      : Meal.Options
   Description : Various functions pertaining filtering meals.
   Copyright   : (c) Tony Zorman  2020 2021 2022 2025
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
  filterOptions, -- :: MealOptions -> Meals -> Meals
) where

import Meal
import Util

import Data.Text qualified as T


-- | Options for filtering meals.
type MealOptions :: Type
data MealOptions = MealOptions
  { mealType :: MealType
  , mealTime :: MealTime
  , ignored  :: [Ignored] -- ^ Ignored stuff
  }

-- | Stuff one can ignore; i.e., meals the user might not want printed.
type Ignored :: Type
data Ignored
  = INotes [Text] -- ^ Ignored notes
  | ICat   [Text] -- ^ Ignored categories
  | IName  [Text] -- ^ Ignored names

-- | What type of meal are we looking for?
type MealType :: Type
data MealType
  = AllMeals
  | Vegetarian
  | Vegan

-- | Which time of day should the meal happen at?
type MealTime :: Type
data MealTime
  = Dinner
  | Lunch
  | AllDay

-- | Filter for the meal options given; ignore anything that's already
-- sold out.
filterOptions :: MealOptions -> Meals -> Meals
filterOptions opts = filter (coerce $ availableOpts opts)

-- | All of the options a user picked.  Every predicate should be
-- satisfied in order for the result to be accepted.
availableOpts :: MealOptions -> Predicate Meal
availableOpts MealOptions{ mealType, mealTime, ignored }
  =  mconcat (coerce [notSoldOut, fitsDiet, correctTimeOfDay])
  <> foldMap' ignoreThing ignored
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

  ignoreThing :: Ignored -> Predicate Meal
  ignoreThing = uncurry foldMap' . \case
    INotes s -> (notPartOfNotes, s)
    ICat   s -> (notCategory,    s)
    IName  s -> (notName,        s)

  notName, notCategory, notPartOfNotes :: Text -> Predicate Meal
  notName        s = coerce $ not . (s `T.isInfixOf`)     . name
  notCategory    s = coerce $       (s /=)                . category
  notPartOfNotes s = coerce $ not . any (s `T.isInfixOf`) . notes

  -- See if meal is vegetarian or there's some sort of vegetarian
  -- variant available.
  vegetarian :: Meal -> Bool
  vegetarian = eitherOf (inNotes "vegetarisch") (inName "vegetarisch")

  -- See if meal is vegan or there's some vegetarian variant available.
  vegan :: Meal -> Bool
  vegan = eitherOf (inNotes "vegan") (inName "vegan")

  dinner :: Meal -> Bool
  dinner = ("Abendangebot" `T.isInfixOf`) . category

  notSoldOut :: Meal -> Bool
  notSoldOut = isJust . prices

  inNotes :: Text -> Meal -> Bool
  inNotes s = any ((s `T.isInfixOf`) . T.toLower) . notes

  inName :: Text -> Meal -> Bool
  inName s = (s `T.isInfixOf`) . T.toLower . name
