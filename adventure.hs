-- Suffer within - game by Jakub Antas, Jan Górski, Patryk Zdziech

import Data.Maybe

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

type Interactables = [String]

data Location = Location
  { name :: String,
    interactables :: Interactables,
    north :: Maybe Location,
    south :: Maybe Location,
    east :: Maybe Location,
    west :: Maybe Location
  }

newtype Flags = Flags
  { -- isBirdKilled :: Bool,
    isHallOpen :: Bool
    -- isEngineOn :: Bool
  }

data State = State {location :: Location, flags :: Flags}

--------------------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- stany -----------------------------------------------------------------------
--------------------------------------------------------------------------------

start = State {location = garden, flags = Flags {isHallOpen = False}}

--------------------------------------------------------------------------------
-- lokalizacje -----------------------------------------------------------------
--------------------------------------------------------------------------------

garden :: Location
garden =
  Location
    { name = "Garden",
      interactables = [],
      north = Just hall,
      south = Nothing,
      east = Just glasshouse,
      west = Nothing
    }

hall :: Location
hall =
  Location
    { name = "Hall",
      interactables = [],
      north = Nothing,
      south = Just garden,
      east = Just banquetHall,
      west = Just library
    }

glasshouse :: Location
glasshouse =
  Location
    { name = "Glasshouse",
      interactables = ["slingshot"],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just garden
    }

library :: Location
library =
  Location
    { name = "Library",
      interactables = ["piece_of_paper", "window", "bool"],
      north = Nothing,
      south = Nothing,
      east = Just hall,
      west = Nothing
    }

banquetHall :: Location
banquetHall =
  Location
    { name = "Banquet Hall",
      interactables = ["painting", "chandelier", "skeletons"],
      north = Nothing,
      south = Nothing,
      east = Just closet,
      west = Just hall
    }

closet :: Location
closet =
  Location
    { name = "Closet",
      interactables = ["crystal ball", "floor"],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just banquetHall
    }

--------------------------------------------------------------------------------
-- FUNKCJE ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

readCommand :: IO String
readCommand = do
  putStr "> "
  xs <- getLine
  return xs

displayInteractables :: Interactables -> IO ()
displayInteractables xs = putStr (unlines ("Interactables:" : xs))

-- Funkcja do zmiany pokoju na podstawie kierunku
move :: Location -> Char -> Location
move currentLocation direction = do
  case direction of
    'n' -> fromMaybe currentLocation (north currentLocation)
    's' -> fromMaybe currentLocation (south currentLocation)
    'e' -> fromMaybe currentLocation (east currentLocation)
    'w' -> fromMaybe currentLocation (west currentLocation)
    _ -> currentLocation

-- Funkcja do wyświetlania pokoju
displayLocation :: Location -> IO ()
displayLocation location = putStrLn $ "You are in the " ++ name location

gameLoop :: State -> IO ()
gameLoop state = do
  let loc = location state
  displayLocation loc

  let inter = interactables loc
  displayInteractables inter

  putStr "Enter direction (n/s/e/w, or 'q' to quit): "
  command <- getLine
  case command of
    "q" -> putStrLn "Goodbye!"
    _ -> do
      let local = foldl move loc command
      let flgs = flags state

      let isopen = isHallOpen flgs

      case local of
        Location "Library" _ _ _ _ _ -> do
          putStrLn "You are in the Library!"
          let whatever = State {location = local, flags = Flags {isHallOpen = False}}
          gameLoop whatever
        _ -> do
          let whatever = State {location = local, flags = flags state}
          gameLoop whatever

main = do
  gameLoop start
