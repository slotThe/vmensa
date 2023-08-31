{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
{- |
   Module      : CLI
   Description : Command line interface for the application.
   Copyright   : (c) Tony Zorman  2020 2021 2022
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module CLI (
  execOptionParser, -- :: IO (Options [Mensa 'NoMeals] DatePP)
  Options (..),
) where

import Meal.Options
import Mensa
import Mensa.PP
import Paths_vmensa (version)
import Time
import Util

import Data.Attoparsec.Text qualified as A
import Data.Map.Strict      qualified as Map

import Data.Attoparsec.Text ((<?>))
import Data.Map.Strict ((!))
import Data.Time.Calendar (Day, DayOfWeek (Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday), fromGregorian)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, auto, execParser, footer, fullDesc, header, help, helper, info, infoOption, long, metavar, option, short, str, switch, value)
import Options.Applicative.CmdLine.Util (AttoParser, aliases, anyOf, anyOfRM, anyOfSkip, attoReadM, optionA, showSepChars, splitOn, splitWith)


-- | Execute the Parser.
execOptionParser :: IO (Options [Mensa 'NoMeals] DatePP)
execOptionParser = do
  opts@Options{ date, mensaOptions } <- execParser options
  iso <- getDate date
  pure $ opts { date = ppDate iso date
              , mensaOptions = mensaOptions
                  { canteen = addDate (tshow iso) <$> canteen mensaOptions }
              }

-- | Global canteen options.  These will double as command line options.
type Options :: Type -> Type -> Type
data Options mensa date = Options
  { mealOptions  :: MealOptions
  , mensaOptions :: MensaOptions mensa
  , date         :: date  -- ^ Access date
  }

-- | Create an info type from the canteen options, adding help text and
-- other nice features.
options :: ParserInfo (Options [Mensa 'Incomplete] Date)
options = info
  (helper <*> versionOpt <*> pOptions)
  (  header "vmensa: Query the Stundentenwerk API from inside your terminal!"
  <> footer ("In accordance to POSIX standards, options really only take a \
            \single argument.  For options where it's intuitive to specify \
            \more than one argument (e.g. '-m'), this is resolved by either \
            \wrapping the argument in quotes, or not using spaces when \
            \separating the input.  Arguments may be separated by any of the \
            \following characters:" ++ showSepChars sepChars)
  <> fullDesc
  )
 where
  versionOpt :: Parser (a -> a)
  versionOpt = infoOption (showVersion version)
     ( long "version"
    <> short 'V'
    <> help "Show version"
     )

-- | Parse all command line options.
pOptions :: Parser (Options [Mensa 'Incomplete] Date)
pOptions = do
  mealOptions <- do
    mealType <- pMealType
    mealTime <- pMealTime
    iKat     <- pIKat
    iNotes   <- pINotes
    ignored  <- pIgnored
    pure MealOptions{..}
  mensaOptions <- do
    canteen  <- pCanteens
    sections <- pSections
    noAdds   <- pNoAdds
    lineWrap <- pLineWrap
    columns  <- pColumns
    pure MensaOptions{..}
  date     <- pDate
  pure Options{..}

pMealType :: Parser MealType
pMealType = option pDiet
   ( long "diet"
  <> short 'd'
  <> metavar "D"
  <> help "Which kinds of meals (all-meals | vegetarian | vegan) to \
          \display.  Defaults to vegan."
  <> value Vegan
   )
 where
  -- Parse user input into a proper 'MealTime'.
  pDiet :: ReadM MealType
  pDiet = anyOfRM
    [ (AllMeals  , ["a"]           )
    , (Vegetarian, ["vege", "vegg"])
    , (Vegan     , ["v"]           )
    ]

-- | Times of day where different meals are available.
pMealTime :: Parser MealTime
pMealTime = option pTime
   ( long "time"
  <> short 't'
  <> metavar "T"
  <> help "Which menu options (lunch | dinner | all-day) to display.  \
          \Defaults to all-day."
  <> value AllDay
   )
 where
  -- Parse user input into a proper 'MealTime'.
  pTime :: ReadM MealTime
  pTime = anyOfRM [(Dinner, ["d"]), (Lunch , ["l"]), (AllDay, ["a"])]

-- | Line wrapping for certain categories only.
pLineWrap :: Parser Natural
pLineWrap = option pWrap
   ( long "wrap"
  <> short 'w'
  <> metavar "N"
  <> help "Wrap the \"Notes\" and \"Essen\" sections at N columns\
          \; minimum: 25 with the exception of 0, which indicates\
          \ no wrapping at all."
  <> value 0  -- No line wrapping.
   )
 where
  pWrap :: ReadM Natural = attoReadM $
    A.decimal >>= \n -> if n == 0 || n >= 25 then pure n else empty
      <?> "pWrap: Argument should be at least 25 (or 0)"

-- | Dates are (optional) arguments.
pDate :: Parser Date
pDate = maybe Today toDate <$> optional (some $ argument str (metavar "DAY"))
 where
  -- Convert all the rest to a date with a default value.
  toDate :: [Text] -> Date
  toDate = fromRight Today . A.parseOnly pDate' . unwords

  -- Parse our entire 'Date' type.
  pDate' :: AttoParser Date
  pDate' = A.choice
    [ Today    <$  A.asciiCI "today"
    , Next     <$> pDay
    , Tomorrow <$  A.asciiCI "t"
    , ISODate  <$> pISODate
    , DMYDate  <$> pDMYDate
    ]

  -- Parse a 'DayOfWeek' using both german and english names.
  pDay :: AttoParser DayOfWeek
  pDay = anyOf
    [ (Monday   , ["mo"]      )
    , (Tuesday  , ["tu", "di"])
    , (Wednesday, ["w" , "mi"])
    , (Thursday , ["th", "do"])
    , (Friday   , ["f"]       )
    , (Saturday , ["sa"]      )
    , (Sunday   , ["su", "so"])
    ]

  pISODate :: AttoParser Day
  pISODate =
    fromGregorian <$> A.decimal <* "-" <*> A.decimal <* "-" <*> A.decimal

  pDMYDate :: AttoParser (Int, Maybe Int, Maybe Integer)
  pDMYDate =
    (,,) <$> A.decimal
         <*> optional (A.space >> fromEnum <$> anyOfSkip (== ' ')
               [ (January  , ["ja"]        )
               , (February , ["f"]         )
               , (March    , ["mar", "mä"] )
               , (April    , ["ap"]        )
               , (May      , ["may", "mai"])
               , (June     , ["jun"]       )
               , (July     , ["jul"]       )
               , (August   , ["au"]        )
               , (September, ["s"]         )
               , (October  , ["o"]         )
               , (November , ["n"]         )
               , (December , ["d"]         )
               ])
         <*> optional (A.space *> A.decimal)

-- | Ignore a certain category of meals.
pIKat :: Parser [Text]
pIKat = optionA (splitOn sepChars)
   ( long "ikat"
  <> metavar "STR"
  <> help "Ignore anything you want from the \"Kategorie\" section.  \
          \DEPRECATED: Use --ignore instead."
  <> value []
   )

-- | Filter out meals due to certain ingredients etc.
pINotes :: Parser [Text]
pINotes = optionA (splitOn sepChars)
   ( long "inotes"
  <> metavar "STR"
  <> help "Ignore anything you want from the \"Notes\" section.  \
          \DEPRECATED: Use --ignore instead."
  <> value []
   )

-- | Filter out things from sections.
pIgnored :: Parser [Ignored]
pIgnored = many $ optionA go
   ( long "ignore"
  <> short 'i'
  <> help "Ignore certain words from a certain section.  \
          \Currently supported sections are name, notes, and category.  \
          \For example: `--ignore cat:this,that,these', `--ignore notes:this,that,these', or `--ignore name:this,that,these'.  \
          \Multiple calls to --ignore are possible."
   )
 where
  go :: AttoParser Ignored
  go = parse "notes:" INotes <|> parse "cat:" ICat <|> parse "name:" IName

  parse :: AttoParser Text -> ([Text] -> Ignored) -> AttoParser Ignored
  parse s d = s *> (d <$> splitOn sepChars)

{- | Canteens the user wants to be shown the meals of.

As command-line argument parsing is a local process, we still don't know
the date here.  Hence, we are returning a 'Mensa' that still wants to
know that information.
-}
pCanteens :: Parser [Mensa 'Incomplete]
pCanteens = optionA (pCanteen `splitWith` sepChars)
   ( long "mensen"
  <> short 'm'
  <> metavar "CANTEENS"
  <> help "Specify the canteens that you want to show.  \
          \Default: Alte,Zeltschlösschen,U-Boot,Siedepunkt."
  <> value (mkEmptyMensa <$> [ (fst (canteens ! 4 ), mensaURL 4 )
                             , (fst (canteens ! 35), mensaURL 35)
                             , (fst (canteens ! 29), mensaURL 29)
                             , (fst (canteens ! 9 ), mensaURL 9 )
                             ])
   )
 where
  pCanteen :: AttoParser (Mensa 'Incomplete)
  pCanteen = mkEmptyMensa . second mensaURL
         <$> A.choice (mkParser <$> Map.keys canteens)
          <* A.skipWhile (`notElem` sepChars)

  -- Construct an empty (i.e. no food to serve) 'Mensa'.
  mkEmptyMensa :: (Text, Text -> Text) -> Mensa 'Incomplete
  mkEmptyMensa = uncurry mkIncompleteMensa

  mkParser :: Int -> AttoParser (Text, Int)
  mkParser k = (name, k) <$ aliases als
   where (name, als) :: (Text, [Text]) = canteens ! k

  -- __All__ available canteens.  We do have a lot of them, apparently.
  canteens :: Map Int (Text, [Text])
  canteens = fromList
    [ (4,  ("Alte Mensa"                     , ["A"]))
    , (6,  ("Mensa Reichenbachstraße"        , ["Re"]))
    , (8,  ("Mensologie"                     , ["Me"]))
    , (9,  ("Mensa Siedepunkt"               , ["Si"]))
    , (10, ("Mensa TellerRandt"              , ["T"]))
    , (11, ("Mensa Palucca Hochschule"       , ["Pal", "Ho"]))
    , (13, ("Mensa Stimm-Gabel"              , ["St", "Ga"]))
    , (24, ("Mensa Kraatschn"                , ["K"]))
    , (25, ("Mensa Mahlwerk"                 , ["Ma"]))
    , (28, ("MiO - Mensa im Osten"           , ["MiO", "Os"]))
    , (29, ("BioMensa U-Boot"                , ["Bio", "U"]))
    , (30, ("Mensa Sport"                    , ["Sport"]))
    , (32, ("Mensa Johannstadt"              , ["Jo"]))
    , (33, ("Mensa WUeins / Sportsbar"       , ["W", "Sports"]))
    , (34, ("Mensa Brühl"                    , ["Br"]))
    , (35, ("Zeltschlösschen"                , ["Z"]))
    , (36, ("Grill Cube"                     , ["Gr", "C"]))
    , (37, ("Pasta-Mobil"                    , ["Pas", "Mo"]))
    , (38, ("Mensa Rothenburg"               , ["Ro"]))
    , (39, ("Mensa Bautzen Polizeihochschule", ["Ba", "Po"]))
    , (42, ("Mensa Oberschmausitz"           , ["Ob"]))
    ]

  -- Template URL for getting all meals of a certain Meals.
  mensaURL
    :: Int   -- Number of the Mensa in the API
    -> Text  -- Date at which we would like to see the food.
    -> Text
  mensaURL num date = mconcat
    [ "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
    , tshow num , "/days/"
    , date      , "/meals"
    ]

-- | Sections to be displayed, making sure that no sections appear twice.
pSections :: Parser [Section]
pSections = nub <$> optionA (pSection `splitWith` sepChars)
   ( long "sections"
  <> short 's'
  <> metavar "S"
  <> help "Sections to print (in order) in the final output."
  <> value [Name, Price, Notes, Category]
   )
 where
  -- Parse user input into a proper 'MealTime'.
  pSection :: AttoParser Section
  pSection = anyOfSkip (`notElem` sepChars)
    [ (Name    , ["na"])
    , (Notes   , ["no"])
    , (Price   , ["p"] )
    , (Category, ["c"] )
    ]

-- | Whether to display letters related to additives in parentheses;
-- like @(A, A1, G)@ and so on.
pNoAdds :: Parser Bool
pNoAdds = switch
   ( long "no-additives"
  <> short 'p'
  <> help "Whether to show the unique letter of additives in parentheses."
   )

-- | Whether canteens should be printed in a n-column layout.
pColumns :: Parser Natural
pColumns = option auto
  (  long "columns"
  <> short 'c'
  <> metavar "N"
  <> help "Whether canteens should be printed in an N-column layout."
  <> value 1
  )

-- | Our separator chars.
sepChars :: [Char]
sepChars = [',', ';', ':', '.']
