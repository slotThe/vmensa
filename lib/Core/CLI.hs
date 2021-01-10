{- |
   Module      : Core.CLI
   Description : Command line interface for the application.
   Copyright   : (c) Tony Zorman, 2020
   License     : GPL-3
   Maintainer  : tonyzorman@mailbox.org
   Stability   : experimental
   Portability : non-portable
-}
module Core.CLI (
    Options (..),
    options,       -- :: ParserInfo Options
) where

import Core.Time (
    Date (DMYDate, ISODate, Next, Today, Tomorrow),
    Month (April, August, December, February, January, July, June, March, May, November, October, September),
 )
import Core.Types
    ( MealTime(AllDay, Dinner, Lunch)
    , MealType(AllMeals, Vegan, Vegetarian)
    , Mensa
    , Section(Category, Name, Notes, Price)
    , mkEmptyMensa
    )
import Paths_vmensa (version)

import qualified Data.Attoparsec.Text as A
import qualified Data.Map             as Map
import qualified Data.Text            as T

import Data.Map ((!))
import Data.Time.Calendar (
    Day,
    DayOfWeek (Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday),
    fromGregorian,
 )
import Options.Applicative (
    Parser,
    ParserInfo,
    ReadM,
    argument,
    auto,
    footer,
    fullDesc,
    header,
    help,
    helper,
    info,
    infoOption,
    long,
    metavar,
    option,
    short,
    str,
    value,
 )
import Options.Applicative.Util (
    aliases,
    anyOf,
    attoReadM,
    showSepChars,
    splitOn,
    splitWith,
 )


-- | Options the user may specify on the command line.
data Options = Options
    { mealType :: !MealType
    , lineWrap :: !Int
    , mealTime :: !MealTime
    , iKat     :: ![Text]
    , iNotes   :: ![Text]
    , canteens :: ![Text -> Mensa]  -- ^ Still waiting for a date.
    , sections :: ![Section]
    , date     :: !Date
    }

-- | Create an info type from our options, adding help text and other
-- nice features.
options :: ParserInfo Options
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
pOptions :: Parser Options
pOptions =  Options
        <$> pMealType
        <*> pLineWrap
        <*> pMealTime
        <*> pIKat
        <*> pINotes
        <*> pCanteens
        <*> pSections
        <*> pDate

pMealType :: Parser MealType
pMealType = option pDiet
     ( long "diet"
    <> short 'd'
    <> metavar "D"
    <> help "Which kinds of meals (all-meals | vegetarian | vegan) to \
            \display.  Defaults to vegetarian."
    <> value Vegetarian
     )
  where
    -- | Parse user input into a proper 'MealTime'.
    pDiet :: ReadM MealType
    pDiet = attoReadM $ anyOf
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
    -- | Parse user input into a proper 'MealTime'.
    pTime :: ReadM MealTime
    pTime = attoReadM $ anyOf [(Dinner, ["d"]), (Lunch , ["l"]), (AllDay, ["a"])]

-- | Line wrapping for certain categories only.
pLineWrap :: Parser Int
pLineWrap = option auto
     ( long "wrap"
    <> short 'w'
    <> metavar "INT"
    <> help "Wrap the \"Notes\" and \"Essen\" sections at INT columns."
    <> value 0  -- No line wrapping.
     )

-- | Dates are (optional) arguments.
pDate :: Parser Date
pDate = toDate . fromMaybe [] <$> optional (some $ argument str (metavar "DAY"))
  where
    -- | Convert all the rest to a date with a default value.
    toDate :: [Text] -> Date
    toDate = fromRight Today . A.parseOnly pDate' . T.unwords

    -- | Parse our entire 'Date' type.
    pDate' :: A.Parser Date
    pDate' = A.choice
        [ Today    <$  A.asciiCI "today"
        , Next     <$> pDay
        , Tomorrow <$  A.asciiCI "t"
        , ISODate  <$> pISODate
        , DMYDate  <$> pDMYDate
        ]

    -- | Parse a 'DayOfWeek' using both german and english names.
    pDay :: A.Parser DayOfWeek
    pDay = anyOf
        [ (Monday   , ["mo"]      )
        , (Tuesday  , ["tu", "di"])
        , (Wednesday, ["w" , "mi"])
        , (Thursday , ["th", "do"])
        , (Friday   , ["f"]       )
        , (Saturday , ["sa"]      )
        , (Sunday   , ["su", "so"])
        ]

    pISODate :: A.Parser Day
    pISODate =
        fromGregorian <$> A.decimal <* "-" <*> A.decimal <* "-" <*> A.decimal

    pDMYDate :: A.Parser (Maybe Integer, Maybe Int, Int)
    pDMYDate = do
        d <- A.decimal
        m <- optional $ A.space >> fromEnum <$> anyOf
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
            ]
        y <- optional $ A.space *> A.decimal
        pure (y, m, d)

-- | Ignore a certain category of meals.
pIKat :: Parser [Text]
pIKat = option (splitOn sepChars)
     ( long "ikat"
    <> metavar "STR"
    <> help "Ignore anything you want from the \"Kategorie\" section."
    <> value []
     )

-- | Filter out meals due to certain ingredients etc.
pINotes :: Parser [Text]
pINotes = option (splitOn sepChars)
     ( long "inotes"
    <> metavar "STR"
    <> help "Ignore anything you want from the \"Notes\" section."
    <> value []
     )

{- | Canteens the user wants to be shown the meals of.

As command-line argument parsing is a local process, we still don't know
the date here.  Hence, we are returning a 'Mensa' that still wants to
know that information.
-}
pCanteens :: Parser [Text -> Mensa]
pCanteens = option ((mkEmptyMensa <$> pCanteen) `splitWith` sepChars)
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
    pCanteen :: A.Parser (Text, Text -> Text)
    pCanteen = second mensaURL <$> A.choice (mkParser <$> Map.keys canteens)
            <* A.skipWhile (`notElem` sepChars)

    mkParser :: Int -> A.Parser (Text, Int)
    mkParser k = (name, k) <$ aliases als
      where (name, als) :: (Text, [Text]) = canteens ! k

    -- | __All__ available canteens.  We do have a lot of them, apparently.
    canteens :: Map Int (Text, [Text])
    canteens = fromList
        [ (4,  ("Alte Mensa"                     , ["A"]))
        , (6,  ("Mensa Reichenbachstraße"        , ["R"]))
        , (8,  ("Mensologie"                     , ["Mensologie"]))
        , (9,  ("Mensa Siedepunkt"               , ["Si"]))
        , (10, ("Mensa TellerRandt"              , ["TellerRandt"]))
        , (11, ("Mensa Palucca Hochschule"       , ["Palucca", "Hochschule"]))
        , (13, ("Mensa Stimm-Gabel"              , ["Stimm", "Gabel"]))
        , (24, ("Mensa Kraatschn"                , ["Kraat"]))
        , (25, ("Mensa Mahlwerk"                 , ["Mahl"]))
        , (28, ("MiO - Mensa im Osten"           , ["MiO", "Osten"]))
        , (29, ("BioMensa U-Boot"                , ["Bio", "U-Boot", "U"]))
        , (30, ("Mensa Sport"                    , ["Sport"]))
        , (32, ("Mensa Johannstadt"              , ["Johannstadt"]))
        , (33, ("Mensa WUeins / Sportsbar"       , ["WUeins", "Sportsbar"]))
        , (34, ("Mensa Brühl"                    , ["Brühl"]))
        , (35, ("Zeltschlösschen"                , ["Z"]))
        , (36, ("Grill Cube"                     , ["Gr", "C"]))
        , (37, ("Pasta-Mobil"                    , ["Pasta-Mobil", "Pasta"]))
        , (38, ("Mensa Rothenburg"               , ["Rothenburg"]))
        , (39, ("Mensa Bautzen Polizeihochschule", ["Bautzen", "Polizeihochschule"]))
        , (42, ("Mensa Oberschmausitz"           , ["Oberschmausitz"]))
        ]

    -- | Template URL for getting all meals of a certain Meals.
    mensaURL
        :: Int   -- ^ Number of the Mensa in the API
        -> Text  -- ^ Date at which we would like to see the food.
        -> Text
    mensaURL num date = mconcat
        [ "https://api.studentenwerk-dresden.de/openmensa/v2/canteens/"
        , tshow num , "/days/"
        , date      , "/meals"
        ]

-- | Sections to be displayed, making sure that no sections appear twice.
pSections :: Parser [Section]
pSections = nub <$> option (pSection `splitWith` sepChars)
     ( long "sections"
    <> short 's'
    <> metavar "S"
    <> help "Sections to print (in order) in the final output."
    <> value [Name, Price, Notes, Category]
     )
  where
    -- | Parse user input into a proper 'MealTime'.
    pSection :: A.Parser Section
    pSection = anyOf [ (Name    , ["na"])
                     , (Notes   , ["no"])
                     , (Price   , ["p"] )
                     , (Category, ["c"] )
                     ]
            <* A.skipWhile (`notElem` sepChars)

-- | Our separator chars.
sepChars :: [Char]
sepChars = [',', ';', ':', '.']
