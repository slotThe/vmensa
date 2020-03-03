{-# LANGUAGE StrictData  #-}
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
    , Date(..)
    , options
    ) where

-- Text
import           Data.Text ( Text )

-- Other imports
import qualified Data.Attoparsec.Text as A
import           Data.Time.Calendar
    ( DayOfWeek(Friday, Monday, Saturday, Sunday, Thursday, Tuesday,
          Wednesday)
    )
import           Options.Applicative
    ( Parser, ParserInfo, (<|>), argument, auto, flag, fullDesc, header, help
    , helper, info, long, metavar, option, short, str, switch, value
    )


-- | Options the user may specify on the command line.
data Options = Options
    { allMeals  :: Bool
    , lineWrap  :: Int
    , mealTime  :: MealTime
    , date      :: Date
    }

-- | Create an info type from our options, adding help text and other nice
-- features.
options :: ParserInfo Options
options = info
    (helper <*> pOptions)  -- create "--help"
    (  header "vmensa: Query the Stundentenwerk API from inside your terminal!"
    <> fullDesc
    )

-- | Parse all command line options.
pOptions :: Parser Options
pOptions = Options
    <$> pAllMeals
    <*> pLineWrap
    <*> pMealTime
    <*> pDate

pAllMeals :: Parser Bool
pAllMeals = switch
     ( long "allmeals"
    <> short 'a'
    <> help "Display all meals (instead of only the vegetarian/vegan one)."
     )

-- | Which time of day should the meal happen at?
data MealTime
    = Dinner
    | Lunch
    | AllDay

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

-- | Line wrapping for certain categories only.
pLineWrap :: Parser Int
pLineWrap = option auto
     ( long "wrap"
    <> short 'w'
    <> metavar "INT"
    <> help "Wrap the \"Notes\" and \"Essen\" sections at INT columns."
    <> value 0  -- No line wrapping.
     )

-- | Type for specifying exactly which day one wants to see the menu for.
data Date
    = Today
    | Tomorrow
    | Next DayOfWeek  -- ^ This will *always* show the next 'DayOfWeek'
                      -- (e.g. calling 'Next Monday' on a monday will result in
                      -- getting the menu for the following monday).
    | Date Text       -- ^ Manual date entry in the format YYYY-MM-DD
    deriving (Show)

-- | Dates are (optional) arguments.
pDate :: Parser Date
pDate = pToDate <$> argument str (metavar "DAY" <> value "today")

-- | Parse user input into a proper 'Date'.
pToDate :: Text -> Date
pToDate input = case A.parseOnly pAttoDate input of
  -- If the parsing fails for some reason, just return today's menu.
  Left  _ -> Today
  Right d -> d

-- | Parse our entire 'Date' type.
pAttoDate :: A.Parser Date
pAttoDate = A.choice
    [ Today <$ A.asciiCI "today"
    , Next <$> pDay
    , Tomorrow <$ A.asciiCI "t"
    , Date <$> A.takeWhile (/= ' ')
    ]

-- | Parse a 'DayOfWeek' using both german and english names.
pDay :: A.Parser DayOfWeek
pDay = A.choice
    [ Monday    <$ aliases [      "mo"]
    , Tuesday   <$ aliases ["tu", "di"]
    , Wednesday <$ aliases ["w" , "mi"]
    , Thursday  <$ aliases ["th", "do"]
    , Friday    <$ aliases [      "f" ]
    , Saturday  <$ aliases [      "sa"]
    , Sunday    <$ aliases ["su", "so"]
    ]

-- | Match on a list of text case-insensitively.
aliases :: [Text] -> A.Parser Text
aliases = foldMap A.asciiCI
