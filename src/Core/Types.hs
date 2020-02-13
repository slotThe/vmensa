{-# LANGUAGE InstanceSigs #-}

{- |
   Module      : Core.Types
   Description : All types needed for JSON parsing the openmensa API
   Copyright   : (c) Tony Zorman, 2019, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.Types
    ( -- * Types for a 'Mensa' and its meals.
      Mensa(..)
    , Meal(..)
    , Prices(..)

    -- * Utility functions.
    , showMensa
    , empty
    )
where

-- Text
import           Data.Text ( Text )
import qualified Data.Text as T

-- Other imports
import Control.Applicative ( (<|>) )
import Data.Aeson          ( FromJSON(parseJSON), Value(Object), (.:) )
import Data.List           ( intersperse )
import GHC.Generics        ( Generic )


-- | Mensa type for a canteen.
newtype Mensa = Mensa [Meal]
    deriving (Generic, FromJSON)

-- | A 'Mensa' is empty if it doensn't have any food to serve.
empty :: Mensa -> Bool
empty (Mensa []) = True
empty _          = False

-- | Type for a meal.
-- PONDER: It might be better to parse this as a generic JSON and then just
-- slurp out the fields that are interesting to us.
data Meal = Meal
    { id       :: Int
    , name     :: Text
    , notes    :: [Text]
    , prices   :: Prices
    , category :: Text
    , image    :: Text  -- ^ Sadly not an image of the actual food :(
    , url      :: Text
    } deriving (Generic, FromJSON)

-- | All the different price types.
data Prices
    = Prices { students  :: Double
             , employees :: Double
             }
    -- Who at the Studentenwerk thought that this was a good idea?
    | NoPrice [Double]

-- | Manually derive `FromJSON` instance due to dumb field names.
instance FromJSON Prices where
    parseJSON (Object v) = Prices
        <$> (v .: "Studierende" <|> v .: "Preis 1")
        <*> (v .: "Bedienstete" <|> v .: "Preis 2")
    parseJSON _ = pure $ NoPrice []

{- | Pretty print only the things I'm interested in.
   This is not a show instance because printing text is faster than printing a
   string.
   See Note [pretty printing]
-}
showMensa
    :: Int    -- ^ Line wrap.
    -> Mensa
    -> Text
showMensa _  (Mensa []) = ""  -- Init is not safe on an empty list.
showMensa lw (Mensa m ) = T.init . T.unlines . map (showMeal lw) $ m
  where
    {- | Pretty print only the things I'm interested in.
       This is not a show instance because printing text is faster than printing a
       string.
    -}
    showMeal
        :: Int   -- ^ Line wrap.
        -> Meal
        -> Text
    showMeal wrap Meal{ category, name, notes, prices } =

           "\n" <> bold "Essen: "     <> name
        <> "\n" <> bold "Preis: "     <> tshow (mstudents prices)
        <> "\n" <> bold "Notes: "     <> umlauts (textWrap wrap notes)
        <> "\n" <> bold "Kategorie: " <> category
      where
        tshow :: (Eq a, Num a, Show a) => a -> Text
        tshow (-1) = "ausverkauft"
        tshow s    = T.pack (show s) <> "€"

        mstudents :: Prices -> Double
        mstudents (Prices  s _) = s
        mstudents (NoPrice _  ) = -1

        -- | For some reason this is only needed on notes.
        umlauts :: Text -> Text
        umlauts = T.replace "&uuml;" "ü"

        -- | Make something bold!
        -- 33 looks nice as well
        -- https://en.wikipedia.org/wiki/ANSI_escape_code#SGR_parameters
        bold :: Text -> Text
        bold s = "\x1b[1m" <> s <> "\x1b[0m"

        -- | Very simple (and probably hilariously inefficient) function to wrap
        -- text at N columns.
        textWrap
            :: Int     -- ^ Max line length
            -> [Text]  -- ^ Text as chunks that have to stay together.
            -> Text    -- ^ Text with line breaks.
        textWrap maxAcc chunks
            | maxAcc == 0 = mconcat $ intersperse ", " chunks
            | otherwise   = go "" 0 chunks
          where
            go :: Text    -- ^ Text with line breaks.
               -> Int     -- ^ Counter of the current line length.
               -> [Text]  -- ^ Text as chunks that have to stay together.
               -> Text
            go l _ [] = l
            go line acc xs@(w:ws)
                | aboveMaxLength = go (line <> "\n")      0            xs
                | otherwise      = go (line <> w <> ", ") (acc + llen) ws
              where
                aboveMaxLength = acc > maxAcc || acc + llen > maxAcc
                llen = T.length w

{- Note [pretty printing]
   ~~~~~~~~~~~~~~~~~~~~~~
   Given a list [a,b], this will print
   @
     a

     b
   @
  instead of
  @
    [a
    b]
  @
  as a normal 'show' would.  The author finds this much easier to read.
-}
