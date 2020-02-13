{- |
   Module      : Core.CLI
   Description : Small command line interface for the application.
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.CLI
    ( Options(..)
    , MealTime(..)
    , options
    ) where

-- Other imports
import Options.Applicative
    ( Parser, ParserInfo, (<**>), (<|>), auto, flag, fullDesc, header, help
    , helper, info, long, metavar, option, short, switch, value
    )


{- | Options the user may specify on the command line.
   Working with boolean values seems easier in this case than to define my own
   data types.
-}
data Options = Options
    { allMeals   :: Bool
    , lineWrap   :: Int
    , mealTime   :: MealTime
    }

-- | Which time of day should the meal happen at?
data MealTime
    = Dinner
    | Lunch
    | AllDay

-- | Create an info type from our options, adding help text and other nice
-- features.
options :: ParserInfo Options
options = info
    (pOptions <**> helper)  -- create "--help"
    (  header "vmensa: Query the Stundentenwerk API from inside your terminal!"
    <> fullDesc
    )

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options
    <$> pAllMeals
    <*> pLineWrap
    <*> pMealTime

pAllMeals :: Parser Bool
pAllMeals = switch
     ( long "allmeals"
    <> short 'a'
    <> help "Display all meals (instead of only the vegetarian/vegan one)."
     )

-- | Times of day where different meals are available.
pMealTime :: Parser MealTime
pMealTime = pDinner <|> pLunch
  where
    -- | Whether to list every meal or only the dinner options.
    pDinner :: Parser MealTime
    pDinner = flag AllDay Dinner
         ( long "dinner"
        <> short 'd'
        <> help "Display only the dinner options."
         )

    -- | Whether to list every meal or only the dinner options.
    pLunch :: Parser MealTime
    pLunch = flag AllDay Lunch
         ( long "lunch"
        <> short 'l'
        <> help "Display only the lunch options."
         )

-- | Parse the 'historyPath' option.  Basically the user may specify an
-- alternative history file to use.
pLineWrap :: Parser Int
pLineWrap = option auto
     ( long "wrap"
    <> short 'w'
    <> metavar "INT"
    <> help "Wrap the \"Notes\" section at INT columns."
    <> value 0  -- No line wrapping.
     )