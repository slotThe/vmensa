{-# LANGUAGE ApplicativeDo #-}
{- |
   Module      : CLI
   Description : Command line interface for the application.
   Copyright   : (c) Tony Zorman  2020 2021 2022 2025
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module CLI (
  execOptionParser,
  Mode(..),
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
import Data.Text.IO         qualified as T

import Data.Attoparsec.Text ((<?>))
import Data.Map.Strict ((!))
import Data.Time.Calendar (Day, DayOfWeek (Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday), fromGregorian)
import Options.Applicative (Parser, ParserInfo, ReadM, argument, auto, execParser, footer, fullDesc, header, help, helper, info, infoOption, long, metavar, option, short, str, switch, value, subparser, command)
import Options.Applicative.CmdLine.Util (AttoParser, aliases, anyOf, anyOfRM, anyOfSkip, attoReadM, optionA, showSepChars, splitOn, splitWith)

type Mode :: Type -> Type -> Type
data Mode mensen date = OpeningTimes | Food (Options mensen date)

-- | Execute the Parser.
execOptionParser :: IO (Mode ([TudMensa 'NoMeals], [UhhMensa 'NoMeals]) DatePP)
execOptionParser = execParser options >>= \case
  -- We pattern match on this here, and then again in main; this is a bit
  -- awkward, but the idea is that we do the IO bits of the parsing here, and
  -- in main we actually do things *with* the parsed data.
  OpeningTimes -> pure OpeningTimes
  Food opts@Options{ date, mensaOptions } -> do
    -- Incompatibilities
    let warning :: Text
        warning = "\x1b[1;31mWARNING:\x1b[0m "
    cs <- if lineWrap mensaOptions == 0 && columns mensaOptions > 1
          then 1 <$ T.putStrLn (warning <> "Multiple columns need a specified line-wrap. \
                                           \Defaulting to a single column…")
          else pure $ columns mensaOptions
    iso <- getDate date
    pure . Food $
      opts { date = ppDate iso date
           , mensaOptions = mensaOptions
               { columns = cs
               , canteen = bimap (map (addDate (tshow iso)))
                                 (map (addDate (tshow iso)))
                                 (canteen mensaOptions)
               }
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
options :: ParserInfo (Mode ([TudMensa 'Incomplete], [UhhMensa 'Incomplete]) Date)
options = info
  (helper <*> versionOpt <*> pMode)
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
pMode :: Parser (Mode ([TudMensa 'Incomplete], [UhhMensa 'Incomplete]) Date)
pMode = A.choice
  [ subparser (command "opening-times" (info (pure OpeningTimes) mempty))
  , Food <$> pOptions
  ]
 where
  pOptions :: Parser (Options ([TudMensa 'Incomplete], [UhhMensa 'Incomplete]) Date)
  pOptions = do
    mealOptions <- do
      mealType <- pMealType
      mealTime <- pMealTime
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
pCanteens :: Parser ([TudMensa 'Incomplete], [UhhMensa 'Incomplete])
pCanteens =
  bimap (map (\(m, _, _) -> TudMensa m  ))
        (map (\(m, _, k) -> UhhMensa m k))
  . partition ((DD ==) . snd3) <$>
      optionA (pCanteen `splitWith` sepChars)
         ( long "mensen"
        <> short 'm'
        <> metavar "CANTEENS"
        <> help "Specify the canteens that you want to show.  \
                \Default: Alte,Zeltschlösschen,U-Boot,Siedepunkt."
        <> value (mkEmptyMensa <$>
                   [ (snd3 (canteens !  4),  4, DD)
                   , (snd3 (canteens ! 35), 35, DD)
                   , (snd3 (canteens ! 29), 29, DD)
                   , (snd3 (canteens !  9),  9, DD)
                   ])
         )
 where
  pCanteen :: AttoParser (Mensa 'Incomplete, Loc, Int)
  pCanteen = mkEmptyMensa
         <$> A.choice (mkParser <$> Map.keys canteens)
          <* A.skipWhile (`notElem` sepChars)

  -- Construct an empty (i.e. no food to serve) 'Mensa'.
  mkEmptyMensa :: (Text, Int, Loc) -> (Mensa 'Incomplete, Loc, Int)
  mkEmptyMensa (n, k, l) = (mkIncompleteMensa n (mensaURL k l), l, k)

  mkParser :: Int -> AttoParser (Text, Int, Loc)
  mkParser k = (name, k, loc) <$ aliases als
   where (loc, name, als) = canteens ! k

  -- __All__ available canteens.  We do have a lot of them, apparently.
  canteens :: Map Int (Loc, Text, [Text])
  canteens = fromList
    [ -- Dresden
      (4,   (DD, "Alte Mensa"                     , ["Alt"]))
    , (6,   (DD, "Mensa Matrix"                   , ["Re", "Mat"]))
    , (8,   (DD, "Mensologie"                     , ["Me"]))
    , (9,   (DD, "Mensa Siedepunkt"               , ["Si"]))
    , (10,  (DD, "Mensa TellerRandt"              , ["T"]))
    , (11,  (DD, "Mensa Palucca Hochschule"       , ["Pal", "Ho"]))
    , (13,  (DD, "Mensa Stimm-Gabel"              , ["Sti", "Ga"]))
    , (24,  (DD, "Mensa Kraatschn"                , ["K"]))
    , (25,  (DD, "Mensa Mahlwerk"                 , ["Mah"]))
    , (28,  (DD, "MiO - Mensa im Osten"           , ["MiO", "Os"]))
    , (29,  (DD, "BioMensa U-Boot"                , ["Bio", "U"]))
    , (30,  (DD, "Mensa Sport"                    , ["Sport"]))
    , (32,  (DD, "Mensa Johannstadt"              , ["Jo"]))
    , (33,  (DD, "Mensa WUeins / Sportsbar"       , ["W", "Sports"]))
    , (34,  (DD, "Mensa Brühl"                    , ["Br"]))
    , (35,  (DD, "Zeltschlösschen"                , ["Zel"]))
    , (36,  (DD, "Grill Cube"                     , ["Gr", "Cu"]))
    , (37,  (DD, "Pasta-Mobil"                    , ["Pas", "Mo"]))
    , (38,  (DD, "Mensa Rothenburg"               , ["Ro"]))
    , (39,  (DD, "Mensa Bautzen Polizeihochschule", ["Ba", "Po"]))
    , (42,  (DD, "Mensa Oberschmausitz"           , ["Ob"]))
    -- Hamburg
    , (137, (HH, "Mensa Studierendenhaus"         , ["Stu"]))
    , (142, (HH, "Mensa Blattwerk"                , ["Bl"]))
    , (143, (HH, "Schlüters (Pizza & More)"       , ["Sc"]))
    , (148, (HH, "Café dell'Arte"                 , ["D"]))
    , (151, (HH, "Mensa Geomatikum"               , ["Ge"]))
    , (154, (HH, "Mensa Philturm"                 , ["Ph"]))
    , (156, (HH, "Mensa Botanischer Garten"       , ["Bo"]))
    , (158, (HH, "Mensa Harbug TU"                , ["Ha"]))
    , (161, (HH, "Mensa Stellingen"               , ["Ste"]))
    , (162, (HH, "Mensa Bucerius Law School"      , ["Buc", "L"]))
    , (164, (HH, "Mensa Finkenau"                 , ["Fi"]))
    , (166, (HH, "Mensa HCU HafenCity"            , ["HCU"]))
    , (168, (HH, "Mensa Bergedorf"                , ["Berg"]))
    , (170, (HH, "Mensa Berliner Tor"             , ["Berl"]))
    , (175, (HH, "Café Jungiusstraße"             , ["J"]))
    , (176, (HH, "Café Alexanderstraße"           , ["Ale"]))
    , (177, (HH, "Café CFEL"                      , ["CF"]))
    , (178, (HH, "Café am Mittelweg"              , ["Mit"]))
    , (179, (HH, "Campus Food Truck"              , ["Fo"]))
    , (383, (HH, "Café ZessP TU"                  , ["Zes"]))
    ]

  -- Template URL for getting all meals of a certain Meals.
  mensaURL
    :: Int  -- Number of the Mensa in the API.
    -> Loc  -- Mensa location.
    -> Text -- Date at which we would like to see the food.
    -> Text
  mensaURL _   HH _    = "https://www.stwhh.de/speiseplan" -- XXX ignore date and number for now
  mensaURL num DD date = mconcat
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
