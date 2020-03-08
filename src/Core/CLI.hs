{- |
   Module      : Core.CLI
   Description : Command line interface for the application.
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
{-# LANGUAGE StrictData  #-}

module Core.CLI
    ( Options(..)
    , MealTime(..)
    , MealType(..)
    , Date(..)
    , options
    ) where

-- Text
import Data.Text (Text)

-- Other imports
import qualified Data.Attoparsec.Text as A
import           Data.Time.Calendar
    ( DayOfWeek(Friday, Monday, Saturday, Sunday, Thursday, Tuesday,
          Wednesday)
    )
import           Options.Applicative
    ( Parser, ParserInfo, argument, auto, fullDesc, header, help, helper, info
    , long, metavar, option, short, str, strOption, value
    )


-- | Options the user may specify on the command line.
data Options = Options
    { mealType :: MealType
    , lineWrap :: Int
    , mealTime :: MealTime
    , iKat     :: [Text]
    , iNotes   :: [Text]
    , date     :: Date
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
    <$> pMealType
    <*> pLineWrap
    <*> pMealTime
    <*> pIKat
    <*> pINotes
    <*> pDate

-- | What type of meal are we looking for?
data MealType
    = AllMeals
    | Vegetarian
    | Vegan

pMealType :: Parser MealType
pMealType = pToMealType <$> strOption
     ( long "diet"
    <> short 'm'
    <> metavar "D"
    <> help "Which kinds of meals do display.  Defaults to vegetarian."
    <> value "vegetarian"
     )
  where
    -- | Parse user input into a proper 'MealTime'.
    pDiet = A.choice
        [ AllMeals   <$ A.asciiCI "a"
        , Vegetarian <$ aliases ["vege", "vegg"]
        , Vegan      <$ A.asciiCI "v"
        ]

    -- | Actually run the parser, with a default in case of a parse failure.
    pToMealType :: Text -> MealType
    pToMealType = parseWithDefault pDiet Vegetarian

-- | Which time of day should the meal happen at?
data MealTime
    = Dinner
    | Lunch
    | AllDay

-- | Times of day where different meals are available.
pMealTime :: Parser MealTime
pMealTime = pToMealTime <$> strOption
     ( long "time"
    <> short 't'
    <> metavar "T"
    <> help "Which menu options (lunch/dinner/all-day) to display.  \
            \Defaults to all-day."
    <> value "all-day"
     )
  where
    -- | Parse user input into a proper 'MealTime'.
    pTime = A.choice
        [ Dinner <$ A.asciiCI "d"
        , Lunch  <$ A.asciiCI "l"
        , AllDay <$ A.asciiCI "a"
        ]

    -- | Actually run the parser, with a default in case of a parse failure.
    pToMealTime :: Text -> MealTime
    pToMealTime = parseWithDefault pTime AllDay

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
                      --   (e.g. calling 'Next Monday' on a monday will result
                      --   in getting the menu for the following monday).
    | Date Text       -- ^ Manual date entry in the format YYYY-MM-DD
    deriving (Show)

-- | Dates are (optional) arguments.
pDate :: Parser Date
pDate = pToDate <$> argument str (metavar "DAY" <> value "today")
  where
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
        [ Monday    <$ A.asciiCI "mo"
        , Tuesday   <$ aliases ["tu", "di"]
        , Wednesday <$ aliases ["w" , "mi"]
        , Thursday  <$ aliases ["th", "do"]
        , Friday    <$ A.asciiCI "f"
        , Saturday  <$ A.asciiCI "sa"
        , Sunday    <$ aliases ["su", "so"]
        ]

    -- | Actually run the parser, with a default in case of a parse failure.
    pToDate :: Text -> Date
    pToDate = parseWithDefault pAttoDate Today

-- | Ignore a certain category of meals.
pIKat :: Parser [Text]
pIKat = parseWithDefault pSplitter [] <$> strOption
     ( long "ikat"
    <> metavar "STR"
    <> help ("Ignore anything you want from the \"Kategorie\" section.  \
            \Note that you need to wrap STR in quotes if you want to ignore \
            \more than one thing.  Options are separated by any of the \
            \following characters:" ++ showSepChars)
    <> value ""
     )

-- | Filter out meals due to certain ingredients etc.
pINotes :: Parser [Text]
pINotes = parseWithDefault pSplitter [] <$> strOption
     ( long "inotes"
    <> metavar "STR"
    <> help ("Ignore anything you want from the \"Notes\" section.  \
            \Note that you need to wrap STR in quotes if you want to ignore \
            \more than one thing.  Options are separated by any of the \
            \following characters:" ++ showSepChars)
    <> value ""
     )

-- | Split a string.
pSplitter :: A.Parser [Text]
pSplitter = A.choice
    [ [] <$ A.endOfInput
    , A.takeWhile (noneOf sepChars)
      `A.sepBy`
      (A.skipSpace *> anyOf sepChars <* A.skipSpace)
    ]
  where
    noneOf :: Eq a => [a] -> a -> Bool
    {-# SPECIALIZE noneOf :: String -> Char -> Bool #-}
    noneOf ws w = all (($ w) . flip (/=)) ws

    anyOf :: String -> A.Parser Char
    anyOf = foldMap A.char

-- | Our separator chars.
sepChars :: String
sepChars = [',', ';', ':', '.']
-- | A small pretty printing function for the separator chars.
showSepChars :: String
showSepChars = concatMap ((" " ++) . show) sepChars

-- | Apply a parser to a given input, if the parsing fails return a default
-- value.
parseWithDefault :: A.Parser p -> p -> Text -> p
parseWithDefault parser def input =
    case A.parseOnly parser input of
        Left  _ -> def
        Right t -> t

-- | Match on a list of text case-insensitively.
aliases :: [Text] -> A.Parser Text
aliases = foldMap A.asciiCI
