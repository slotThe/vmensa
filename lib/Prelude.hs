{- |
   Module      : Prelude
   Description : Custom prelude for the project
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Prelude
    ( module BasePrelude
    , Text
    , Map
    , fromList
    , toList

    , tshow     -- :: Show a => a -> Text
    , fst3      -- :: (a, b, c) -> a
    , eitherOf  -- :: Applicative f => f Bool -> f Bool -> f Bool
    ) where

import BasePrelude hiding (empty, option, toList)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Exts (fromList, toList)

import qualified Data.Text as T


-- | Showing text things.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Get the first element out of a 3-tuple.
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

-- | See if some predicate holds for at least one of the inputs.
eitherOf :: Applicative f => f Bool -> f Bool -> f Bool
eitherOf = liftA2 (||)
