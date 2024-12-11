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
    description :: String,
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
      description = "You are in the garden.\n"++
      "The garden is overgrown, with thorny\n"++
      "bushes and dead leaves scattered across the narrow paths.\n"++
      "A faint mist hangs in the air, making everything feel quiet\n"++
      "and strange. To the north stands a large, dark mansion.\n"++
      "Its stone walls are cracked, covered in ivy, and the windows\n"++
      "are pitch black, as if hiding secrets inside.\n"++
      "To the east, there's an old greenhouse with broken\n"++
      "and foggy glass panels. Inside, twisted plants and vines\n"++
      "press against the glass, and a faint smell of earth and decay\n"++
      "drifts out, hinting at something forgotten within.\n",

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
      description = "You stand at the entrance of the mansion,\n"++
      "the hall before you looming in darkness.\n"++
      "The air is thick with the smell of age and decay,\n"++
      "and the floor creaks under your feet as you step\n"++
      "forward. Before you stand enormous doors,\n"++
      "their surfaces covered in strange, ancient symbols.\n"++
      "The carvings twist and writhe as if alive,\n"++
      "but something is wrong – two pieces of the door\n"++
      "are missing, leaving jagged gaps that seem to pulse\n"++
      "with a faint, unnatural glow. The emptiness between the\n"++
      "gaps feels wrong, as if something is waiting,\n"++
      "something that has been sealed away for a long time.\n"++
      "The silence is oppressive, and the feeling that you\n"++
      "are being watched from the shadows is impossible to shake.\n",

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
      description = "You are in the glasshouse. Shattered glass\n" ++
      "and broken panes lie scattered on the ground.\n" ++
      "Vines crawl over everything, choking what\n" ++
      "remains of the plants. High above, a skeleton\n" ++
      "swings from a fraying rope, its empty eyes\n" ++
      "staring into nothing. Between its teeth\n" ++
      "glimmers a faint, glowing object.\n" ++
      "There has to be a way to get it...\n" ++
      "The air is thick with decay,\n" ++
      "and the silence feels suffocating, broken only\n" ++
      "by the creaking of the hanging bones.",
      
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
      description = "You are in the library.\n" ++
      "The library is cold and dim, filled with shelves of\n" ++
      "forgotten, weathered books. The air is thick with the\n" ++
      "smell of dust and dampness. A broken window lets\n" ++
      "in faint light, casting long shadows on the creaky\n" ++
      "floor. By the window stands a desk, its surface\n" ++
      "covered in dust. On it lies a torn scrap of paper,\n" ++
      "scribbled with frantic, barely legible writing. Nearby,\n" ++
      "a rusted clock stands still, its hands frozen in\n" ++
      "time. In the corner, a spider's web stretches\n" ++
      "between the bookshelves. The room feels heavy with\n" ++
      "unspoken secrets, as if the books themselves are\n" ++
      "hiding dark stories waiting to be uncovered.",

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
      description = "You are in the banquet hall.\n" ++
      "The banquet hall is vast, filled with the stench of decay.\n" ++
      "Long tables are covered in the bodies of the\n" ++
      "dead, some still seated, others slumped on\n" ++
      "the floor. Ashtrays overflow with cigarette butts, their\n" ++
      "ashes still smoldering. Faded paintings hang\n" ++
      "crookedly on the walls, and a broken chandelier casts\n" ++
      "fractured shadows above. The silence is heavy,\n" ++
      "disturbed only by the soft creak of the wooden floor\n" ++
      "beneath your feet.",

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
      description = "You are in the closet.\n" ++
      "The closet is cramped and cluttered, filled with shelves of scattered items.\n" ++
      "Old boxes, forgotten dishes, rusted tools, and scraps of fabric cover every surface.\n" ++
      "On the floor, a pile of unsorted books lies forgotten, while an old dusty trunk sits.\n" ++
      "In the corner stands a strange crystal ball, inside which something unsettling moves.\n" ++
      "Faint wisps of mist swirl within, occasionally forming shapes as if something is trying to escape.\n" ++
      "The air smells of dampness and mildew, and the darkness seems to swallow every corner.\n" ++
      "The place holds more secrets than it lets on, hidden beneath layers of dust and shadow.",

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
displayLocation loc = putStrLn $ description loc

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
    then putStrLn "You close the window. It becomes quiet."
    else putStrLn "You open the window. You can hear the wind blowing from outside."
  let updatedFlags = (flags state) {isWindowOpen = not windowState}
  return state {flags = updatedFlags}
interactWith "painting" state = do
  putStrLn "In the painting: A thick fog blankets a dark, twisted forest"
  putStrLn "Gnarled trees loom like dark silhouettes, their branches"
  putStrLn "reaching out like claws. In the distance, a decaying"
  putStrLn "mansion stands, its broken windows resembling"
  putStrLn "hollow eyes. The faint glow of a flickering lantern"
  putStrLn "casts an eerie light on a narrow, winding path"
  putStrLn "leading toward the mansion, swallowed by the thick darkness."
  return state
interactWith "chandelier" state = do
  putStrLn "The broken chandelier swings, its shattered crystals casting"
  putStrLn "twisted shadows in the dim light – better not walk beneath it."
  return state
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
