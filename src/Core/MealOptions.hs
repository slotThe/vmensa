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
      filterOptions

    -- * Different filters for meals
    , veggie
    , dinner
    , lunch
    ) where

-- Local imports
import Core.Types (Meal(category, notes, prices), Meals, Prices(NoPrice))

-- Text
import qualified Data.Text as T

-- Other imports
import Control.Applicative (liftA2)


-- | Filter for the meal options given, exclude anything that's already sold
-- out.
filterOptions :: [Meal -> Bool] -> Meals -> Meals
filterOptions opts = filter availableOpts
  where
    availableOpts :: Meal -> Bool
    availableOpts = liftA2 (&&) notSoldOut (allOpts opts)

    -- | Every predicate should be satisfied in order for the result to be
    -- accepted.
    allOpts :: [Meal -> Bool] -> Meal -> Bool
    allOpts os meal = all ($ meal) os
{-# INLINE filterOptions #-}

-- | Most of the time we want both.
veggie :: Meal -> Bool
veggie = liftA2 (||) vegan vegetarian
{-# INLINE veggie #-}

vegetarian :: Meal -> Bool
vegetarian = ("Menü ist vegetarisch" `elem`) . notes
{-# INLINE vegetarian #-}

vegan :: Meal -> Bool
vegan = ("Menü ist vegan" `elem`) . notes
{-# INLINE vegan #-}

dinner :: Meal -> Bool
dinner = ("Abendangebot" `T.isInfixOf`) . category
{-# INLINE dinner #-}

lunch :: Meal -> Bool
lunch = not . dinner
{-# INLINE lunch #-}

notSoldOut :: Meal -> Bool
notSoldOut = available . prices
{-# INLINE notSoldOut #-}

available :: Prices -> Bool
available (NoPrice _) = False
available _           = True
