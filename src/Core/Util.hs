{- |
   Module      : Util
   Description : Utility functions
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.Util
    ( tshow
    ) where

-- Text
import           Data.Text ( Text )
import qualified Data.Text as T


-- | Helper function for showing things.
tshow :: Show a => a -> Text
tshow = T.pack . show
