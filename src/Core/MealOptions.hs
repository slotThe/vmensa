{- |
   Module      : MealOptions
   Description : Various functions pertaining to which meals should be displayed
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}

module Core.MealOptions
    ( -- * Filter for the given options
      filterOptions  -- :: Options -> Meals -> Meals
    ) where

import Core.CLI as CLI
    ( MealTime(AllDay, Dinner, Lunch)
    , MealType(AllMeals, Vegan, Vegetarian)
    , Options(Options, iKat, iNotes, mealTime, mealType)
    )
import Core.Types (Meal(category, notes, prices), Meals, Prices(NoPrice))

import qualified Data.Text as T


-- | Filter for the meal options given, ignore anything that's already sold out.
filterOptions :: Options -> Meals -> Meals
filterOptions opts = filter availableOpts
  where
    availableOpts :: Meal -> Bool
    availableOpts = allTrue $ getAllOpts opts

    -- | Every predicate should be satisfied in order for the result to be
    -- accepted.
    allTrue :: [Meal -> Bool] -> Meal -> Bool
    allTrue os meal = all ($ meal) os

    -- | All of the options a user picked.
    getAllOpts :: Options -> [Meal -> Bool]
    getAllOpts Options{ mealType, mealTime, iKat, iNotes } =
        concat
            [ [notSoldOut, mtype, mtime]
            , map notCategory    iKat
            , map notPartOfNotes iNotes
            ]
      where
        mtype = case mealType of
            Vegetarian -> veggie
            Vegan      -> vegan
            AllMeals   -> const True
        mtime = case mealTime of
            AllDay -> const True
            Dinner -> dinner
            Lunch  -> lunch

-- | Most of the time we want both.
veggie :: Meal -> Bool
veggie = (||) <$> vegan <*> vegetarian

vegetarian :: Meal -> Bool
vegetarian = elemNotes "Menü ist vegetarisch"

vegan :: Meal -> Bool
vegan = elemNotes "Menü ist vegan"

dinner :: Meal -> Bool
dinner = ("Abendangebot" `T.isInfixOf`) . category

lunch :: Meal -> Bool
lunch = not . dinner

notSoldOut :: Meal -> Bool
notSoldOut = available . prices

elemNotes :: Text -> Meal -> Bool
elemNotes s = (s `elem`) . notes

notPartOfNotes :: Text -> Meal -> Bool
notPartOfNotes s = not . any (s `T.isInfixOf`) . notes

notCategory :: Text -> Meal -> Bool
notCategory s = (s /=) . category

available :: Prices -> Bool
available = \case
    NoPrice -> False
    _       -> True
