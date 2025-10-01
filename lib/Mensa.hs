{- |
   Module      : Mensa
   Description : The representation for a generic canteen
   Copyright   : (c) Tony Zorman  2019 2020 2021 2022 2025
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Mensa (
  -- * Types for 'Mensa' and its meals.
  Mensa,
  MensaState (..),
  Loc (..),
  LocMensa (..), UhhMensa, TudMensa,

  -- * Setters
  mkIncompleteMensa,

  -- * Transformations
  addMeals,
  addDate,
  mapMensa,

  -- * Getters
  mensaName,
  url,
  meals,
  asMensa,
) where

import Meal
import Util hiding (Prefix)

-- | All of the states a canteen can be in.
type MensaState :: Type
data MensaState where
  -- | An /incomplete/ canteen: still missing a date on which to query
  -- meals, as well as available meals.
  Incomplete :: MensaState
  -- | A canteen with /no meals/: knows when to query for meals but has
  -- not done that yet.
  NoMeals    :: MensaState
  -- | A /complete/ canteen: has a list of meals that it serves on the
  -- given date.
  Complete   :: MensaState

-- | A canteen.
type Mensa :: MensaState -> Type
data Mensa state where
  IncompleteMensa :: Text -> (Text -> Text)          -> Mensa 'Incomplete
  NoMealsMensa    :: Text -> Text                    -> Mensa 'NoMeals
  CompleteMensa   :: Text -> Text           -> Meals -> Mensa 'Complete

mkIncompleteMensa :: Text -> (Text -> Text) -> Mensa 'Incomplete
mkIncompleteMensa = IncompleteMensa

-- | Add meals to a canteen.
addMeals :: Meals -> Mensa 'NoMeals -> Mensa 'Complete
addMeals ms (NoMealsMensa n u) = CompleteMensa n u ms

-- | Get the name of a canteen.
mensaName :: Mensa a -> Text
mensaName = \case
  IncompleteMensa n _   -> n
  NoMealsMensa    n _   -> n
  CompleteMensa   n _ _ -> n

-- | Get a finished URL for a canteen.
url :: Either (Mensa 'NoMeals) (Mensa 'Complete) -> Text
url = \case
  Left  (NoMealsMensa  _ u  ) -> u
  Right (CompleteMensa _ u _) -> u

-- | Extract the meals out of a canteen.
meals :: Mensa 'Complete -> Meals
meals (CompleteMensa _ _ m) = m

-- | Add a date to an incomplete canteen, so that it knows when to fetch
-- meals.
addDate :: Text -> Mensa 'Incomplete -> Mensa 'NoMeals
addDate d (IncompleteMensa n f) = NoMealsMensa n (f d)

-- | A location for a canteen.
type Loc :: Type
data Loc = DD | HH
  deriving stock (Eq)

-- | A canteen with some extra location data.
type LocMensa :: MensaState -> Loc -> Type
data LocMensa s l where
  TudMensa :: Mensa s        -> LocMensa s 'DD
  UhhMensa :: Mensa s -> Int -> LocMensa s 'HH

type TudMensa :: MensaState -> Type
type TudMensa s = LocMensa s 'DD

type UhhMensa :: MensaState -> Type
type UhhMensa s = LocMensa s 'HH

-- | Forget the location of a canteen.
asMensa :: LocMensa s l -> Mensa s
asMensa = \case
  TudMensa m   -> m
  UhhMensa m _ -> m

mapMensa :: (Mensa s -> Mensa s') -> LocMensa s l -> LocMensa s' l
mapMensa f = \case
  TudMensa m   -> TudMensa (f m)
  UhhMensa m i -> UhhMensa (f m) i
