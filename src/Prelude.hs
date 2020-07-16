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
    , tshow
    ) where

import BasePrelude hiding (empty, option)
import Data.Text (Text)

import qualified Data.Text as T


-- | Showing text things.
tshow :: Show a => a -> Text
tshow = T.pack . show
