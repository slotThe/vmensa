{- |
   Module      : Core.CLI
   Description : Small command line interface for the application.
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.CLI where

-- Other imports
import Options.Applicative


data Diet
    = Vegetarian
    | Omnivore

data Dinner
    = All
    | Dinner

data Lunch
    = All'
    | Lunch

-- | Options the user may specify on the command line.
data Options = Options
    { allMeals   :: Diet
    , onlyDinner :: Dinner
    , onlyLunch  :: Lunch
    }

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options
    <$> pAllMeals
    <*> pDinner
    <*> pLunch

pAllMeals :: Parser Diet
pAllMeals = flag Vegetarian Omnivore
     ( long "allmeals"
    <> short 'a'
    <> help "Display all meals (instead of only the vegetarian/vegan one)."
     )

-- | Whether to list every meal or only the dinner options.
pDinner :: Parser Dinner
pDinner = flag All Dinner
     ( long "dinner"
    <> short 'd'
    <> help "Display only the dinner options."
     )

-- | Whether to list every meal or only the dinner options.
pLunch :: Parser Lunch
pLunch = flag All' Lunch
     ( long "lunch"
    <> short 'l'
    <> help "Display only the lunch options."
     )

-- | Create an info type from our options, adding help text and other nice
-- features.
options :: ParserInfo Options
options = info
    (pOptions <**> helper)  -- create "--help"
    (  header "vmensa: Query the Stundentenwerk API from inside your terminal!"
    <> fullDesc
    )
