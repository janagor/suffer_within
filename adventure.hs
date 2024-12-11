import Data.Maybe
import System.Random
import Control.Monad (foldM)

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

type Inventory = [String]
type Interactables = [String]

data Location = Location
  { name :: String,
    interactables :: Interactables,
    items :: [String], -- Rzeczy do podniesienia w lokacji
    north :: Maybe Location,
    south :: Maybe Location,
    east :: Maybe Location,
    west :: Maybe Location
  }

data Flags = Flags
  { isHallOpen :: Bool,
    isBirdKilled :: Bool,
    hasPlayedSlingshot :: Bool, -- Flaga oznaczająca, czy minigra została już rozegrana
    isWindowOpen :: Bool -- Flaga oznaczająca stan okna
  }

data State = State
  { location :: Location,
    flags :: Flags,
    inventory :: Inventory -- Ekwipunek
  }

--------------------------------------------------------------------------------
-- CONSTANTS -------------------------------------------------------------------
--------------------------------------------------------------------------------

start = State {location = garden, flags = Flags {isHallOpen = False, isBirdKilled = False, hasPlayedSlingshot = False, isWindowOpen = False}, inventory = []}

--------------------------------------------------------------------------------
-- lokalizacje -----------------------------------------------------------------
--------------------------------------------------------------------------------

garden :: Location
garden =
  Location
    { name = "Garden",
      interactables = [],
      items = [],
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
      items = [],
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
      items = [],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just garden
    }

library :: Location
library =
  Location
    { name = "Library",
      interactables = ["piece_of_paper", "window", "book"],
      items = [],
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
      items = [],
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
      items = [],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just banquetHall
    }

--------------------------------------------------------------------------------
-- FUNKCJE ---------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Funkcja sprawdzająca, czy gracz może przejść w danym kierunku
canMove :: State -> Char -> Bool
canMove state direction
  | name (location state) == "Garden" && direction == 'n' =
      "golden_key" `elem` inventory state
  | otherwise = True

-- Funkcja do zmiany pokoju na podstawie kierunku z uwzględnieniem ograniczeń
move :: State -> Char -> IO State
move state direction =
  if canMove state direction
    then do
      let newLoc = case direction of
            'n' -> fromMaybe (location state) (north (location state))
            's' -> fromMaybe (location state) (south (location state))
            'e' -> fromMaybe (location state) (east (location state))
            'w' -> fromMaybe (location state) (west (location state))
            _ -> location state
      return state {location = newLoc}
    else do
      putStrLn "You need the golden key to go north from the Garden!"
      return state

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)

readCommand :: IO String
readCommand = do
  putStr "> "
  xs <- getLine
  return xs

displayInteractables :: Interactables -> IO ()
displayInteractables xs = putStr (unlines ("Interactables here:" : xs))

displayItems :: [String] -> IO ()
displayItems xs = putStr (unlines ("Items here:" : xs))

displayInventory :: Inventory -> IO ()
displayInventory xs = putStr (unlines ("Inventory:" : xs))

-- Wyświetlanie obecnej lokalizacji
displayLocation :: Location -> IO ()
displayLocation loc = putStrLn $ "You are in the " ++ name loc

-- Obsługa interakcji
interactWith :: String -> State -> IO State
interactWith "slingshot" state =
  if hasPlayedSlingshot (flags state)
    then do
      putStrLn "You have already played the slingshot game."
      return state
    else do
      newState <- playGuessingGame state
      let newLoc = (location newState) {interactables = filter (/= "slingshot") (interactables (location newState))}
      return newState {location = newLoc}
interactWith "piece_of_paper" state = do
  putStrLn ""
  putStrLn "I am a sequence where each number grows"
  putStrLn "By adding the two that came before it, it shows"
  putStrLn "Find the pair just under 200s crest"
  putStrLn "The penultimate and ultimate in my test"
  putStrLn ""
  return state
interactWith "window" state = do
  let windowState = isWindowOpen (flags state)
  if windowState
    then putStrLn "Window is now closed."
    else putStrLn "Window is now open."
  let updatedFlags = (flags state) {isWindowOpen = not windowState}
  return state {flags = updatedFlags}
interactWith item state = do
  putStrLn $ "You cannot interact with " ++ item ++ "."
  return state

-- Podnoszenie przedmiotów
pickUpItem :: String -> State -> IO State
pickUpItem item state =
  if item `elem` items (location state)
    then do
      putStrLn $ "You picked up " ++ item ++ "."
      let newItems = filter (/= item) (items (location state))
      let newLoc = (location state) {items = newItems}
      let newInv = item : inventory state
      return state {location = newLoc, inventory = newInv}
    else do
      putStrLn "This item is not here."
      return state

-- Minigierka zgadywania liczby
playGuessingGame :: State -> IO State
playGuessingGame state = do
  target <- randomRIO (0, 90) -- Losowanie liczby
  putStrLn "Welcome to the slingshot game! Guess a number between 0 and 90."
  guessingLoop target 0 state

guessingLoop :: Int -> Int -> State -> IO State
guessingLoop target attempts state = do
  putStr "Enter your guess: "
  guessStr <- getLine
  let guess = read guessStr :: Int
  let newAttempts = attempts + 1
  if guess < target
    then do
      putStrLn "Too low!"
      guessingLoop target newAttempts state
    else if guess > target
      then do
        putStrLn "Too high!"
        guessingLoop target newAttempts state
      else do
        putStrLn "Congratulations! You guessed the number!"
        let newLoc = if name (location state) == "Glasshouse"
                       then (location state) {items = "golden_key" : items (location state)}
                       else location state
        let newFlags = (flags state) {hasPlayedSlingshot = True}
        if newAttempts >= 7
          then do
            putStrLn "You took too many shots... The bird is killed!"
            let updatedFlags = newFlags {isBirdKilled = True}
            return state {location = newLoc, flags = updatedFlags}
          else return state {location = newLoc, flags = newFlags}

-- Pętla gry
gameLoop :: State -> IO ()
gameLoop state = do
  putStrLn "--------------------------------------------------"
  let loc = location state
  displayLocation loc
  putStrLn ""

  let inter = interactables loc
  displayInteractables inter
  putStrLn ""

  let itemsHere = items loc
  displayItems itemsHere
  putStrLn ""

  displayInventory (inventory state)
  putStrLn ""

  putStr "Enter command (n/s/e/w to move, 'interact <item>', 'pickup <item>', or 'q' to quit): "
  command <- getLine
  putStrLn ""
  case words command of
    ["q"] -> putStrLn "Goodbye!"
    ["interact", item] -> do
      newState <- interactWith item state
      gameLoop newState
    ["pickup", item] -> do
      newState <- pickUpItem item state
      gameLoop newState
    [dir] -> do
      newState <- foldM move state dir
      gameLoop newState
    _ -> do
      putStrLn "Invalid command."
      gameLoop state

main = do
  gameLoop start
