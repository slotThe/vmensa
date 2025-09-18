{- |
   Module      : Meal
   Description : Everything meals
   Copyright   : (c) Tony Zorman, 2021 2025
   License     : GPL-3
   Maintainer  : Tony Zorman <tonyzorman@mailbox.org>
   Stability   : experimental
   Portability : non-portable
-}
module Meal (
  Meal (..),      -- instances: Generic, FromJSON
  Meals,          -- types alias: [Meal]
  Prices (..),    -- instance FromJSON (Maybe Price)
) where

import Util

import Data.Aeson (FromJSON (parseJSON), Value (Object), (.:))
import Data.Aeson.Types (Parser)


-- | Type for a single meal.  Note that we are only specifying the
-- contents of the JSON that we will actually use.
type Meal :: Type
data Meal = Meal
  { name     :: Text
  , notes    :: [Text]
  , prices   :: Maybe Prices
  , category :: Text
  }
  deriving stock    (Generic, Show)
  deriving anyclass (FromJSON)

-- | A canteen serves food!
type Meals :: Type
type Meals = [Meal]

-- | All the different price types.  Note again that we are only
-- specifying the contents of the JSON that we will actually use.
type Prices :: Type
data Prices = Prices { student :: Double, employee :: Double }
  deriving stock (Show)

instance FromJSON Prices where
  parseJSON :: Value -> Parser Prices
  parseJSON = \case
    Object v -> Prices <$> (v .: "Studierende" <|> v .: "Preis 1")
                       <*> (v .: "Bedienstete" <|> v .: "Preis 2")
    _ -> error "Prices.FromJSON: not a price"
