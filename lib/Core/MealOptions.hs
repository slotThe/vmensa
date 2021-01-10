{- |
   Module      : MealOptions
   Description : Various functions pertaining to which meals should be displayed
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Core.MealOptions (
    -- * Filter for the given options
    filterOptions,  -- :: Options -> Meals -> Meals
) where

import Core.CLI (Options (Options, iKat, iNotes, mealTime, mealType))
import Core.Types (
    Meal (category, name, notes, prices),
    MealTime (AllDay, Dinner, Lunch),
    MealType (AllMeals, Vegan, Vegetarian),
    Meals,
    Prices (SoldOut),
 )

import qualified Data.Text as T


-- | Filter for the meal options given; ignore anything that's already
-- sold out.
filterOptions :: Options -> Meals -> Meals
filterOptions opts = filter availableOpts
  where
    -- | Every predicate should be satisfied in order for the result to
    -- be accepted.
    availableOpts :: Meal -> Bool
    availableOpts meal = all ($ meal) (getAllOpts opts)

-- | All of the options a user picked.
getAllOpts :: Options -> [Meal -> Bool]
getAllOpts Options{ mealType, mealTime, iKat, iNotes } =
    concat
        [ [notSoldOut, fitsDiet, correctTimeOfDay]
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

    notCategory :: Text -> Meal -> Bool
    notCategory s = (s /=) . category

    notPartOfNotes :: Text -> Meal -> Bool
    notPartOfNotes s = not . any (s `T.isInfixOf`) . notes

    -- | See if meal is vegetarian or there's some sort of vegetarian
    -- variant available.
    vegetarian :: Meal -> Bool
    vegetarian = eitherOf (inNotes "Menü ist vegetarisch") (inName "vegetarisch")

    -- | See if meal is vegan or there's some vegetarian variant
    -- available.
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
