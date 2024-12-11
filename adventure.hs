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
      north = Just low_security_prison,
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

      interactables = ["crystal_ball", "floor"],
      items = [],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just banquetHall
    }

low_security_prison :: Location
low_security_prison =
  Location
    { name = "Low Security Prison",
      description = "You are in the low security prison.\n" ++
      "The room is dimly lit, with two small cells on either\n" ++
      "side, their rusted barred doors creaking. The air is\n" ++
      "stale, filled with the scent of mold and cold stone.\n" ++
      "Ahead, through open metal doors, lies another\n" ++
      "similar room, and the sound of dripping water\n" ++
      "echoes through the silence. The oppressive stillness\n" ++
      "makes the space feel even more confining.\n",

      interactables = [],
      items = [],
      north = Just medium_security_prison,
      south = Just hall,  -- Can go back to the hall
      east = Just lust_cell,
      west = Just gluttony_cell
    }

gluttony_cell :: Location
gluttony_cell =
  Location
    { name = "Gluttony Cell",
      description = "You are in the gluttony cell.\n" ++
      "The room reeks of spoiled food and decay. Cracked\n" ++
      "plates and tarnished cutlery are scattered\n" ++
      "around, some half-buried in mold. A rusted bedframe\n" ++
      "lies under piles of rotting food, and the floor is\n" ++
      "sticky with spilled wine and spoiled meat. In the\n" ++
      "corner, a chest overflows with scraps of fabric,\n" ++
      "bones, and remnants of feasts. The air is thick\n" ++
      "with the stench of excess and decay, a grim reminder\n" ++
      "of gluttony’s punishment.\n",

      interactables = ["rotting_food", "floor", "chest"],
      items = [],
      east = Just low_security_prison,  -- Can go back to low_security_prison
      west = Nothing,  -- No further west
      north = Nothing,
      south = Nothing
    }

lust_cell :: Location
lust_cell =
  Location
    { name = "Lust Cell",
      description = "You are in the lust cell.\n" ++
      "The room is dim and heavy with the scent of\n" ++
      "perfume and stale sweat. Faded tapestries hang on the\n" ++
      "walls, depicting intimate scenes. A narrow bed with\n" ++
      "torn silk sheets sits in the center, surrounded by\n" ++
      "broken mirrors reflecting fragmented images. In the\n" ++
      "corner, an old vanity with twisted jewelry and\n" ++
      "faded love letters adds to the oppressive atmosphere,\n" ++
      "filled with the remnants of unchecked desire.\n" ++
      "In the corner, there is a magnetic card held by a\n" ++
      "large electric magnet. There is no way to take it\n" ++
      "while this magnet is powered on.\n",

      interactables = ["magnet", "tapestry", "broken_mirrors"],
      items = [],
      west = Just low_security_prison,  -- Can go back to low_security_prison
      east = Nothing,  -- No further east
      north = Nothing,
      south = Nothing
    }

medium_security_prison :: Location
medium_security_prison =
  Location
    { name = "Medium Security Prison",
      description = "You are in the medium security prison.\n" ++
      "The room is cold, with faded grey stone walls. Two\n" ++
      "empty cells stand on either side, their bars casting\n" ++
      "long shadows. A metal door leads to another similar\n" ++
      "room, behind heavy iron gates. The floor is\n" ++
      "cracked and stained, and dim light from a flickering\n" ++
      "bulb casts eerie shadows across the space\n" ++
      "giving the room a heavy, oppressive feeling.\n",

      interactables = [],
      items = [],
      north = Just high_security_prison, 
      south = Just low_security_prison,  -- Link back to low_security_prison
      east = Just sloth_cell,
      west = Just envy_cell
    }

envy_cell :: Location
envy_cell =
  Location
    { name = "Envy Cell",
      description = "You are in the envy cell.\n" ++
      "The room is cold and empty, with cracked walls\n" ++
      "closing in. A narrow cot sits against one wall,\n" ++
      "covered by a thin, tattered blanket and a notebook on it. A small,\n" ++
      "tarnished mirror on the opposite wall distorts the\n" ++
      "reflection. The air feels thick with bitterness, and\n" ++
      "a small window offers only a sliver of the outside\n" ++
      "world. The floor is covered in a layer of dust,\n" ++
      "untouched, as if no one has ever truly rested here.\n",

      interactables = ["blanket", "notebook"],
      items = [],
      north = Nothing,
      south = Nothing,
      east = Just medium_security_prison,  -- Link back to medium_security_prison
      west = Nothing  -- No further west
    }

sloth_cell :: Location
sloth_cell =
  Location
    { name = "Sloth Cell",
      description = "You are in the sloth cell.\n" ++
      "The room is dim and neglected, with a sagging\n" ++
      "bed in the corner. Empty containers and crumpled\n" ++
      "papers litter the floor. A cracked lamp barely\n" ++
      "lights the space, and the air is stale, untouched by\n" ++
      "effort or care.\n",

      interactables = ["paper", "written_paper"],
      items = ["unlit_torch"],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just medium_security_prison  -- Link back to medium_security_prison
    }

high_security_prison :: Location
high_security_prison =
  Location
    { name = "High Security Prison",
      description = "You are in the high security prison.\n" ++
      "The room is stark and oppressive, with an atmosphere\n" ++
      "of cold metal and concrete. To the right, a cell\n" ++
      "stands behind thick, reinforced bars, making it clear\n" ++
      "this is a place of high security. To the left, heavy\n" ++
      "steel doors loom, their solid form unyielding and\n" ++
      "intimidating. In front of you, there is another set of\n" ++
      "doors, even more imposing, with a magnetic card\n" ++
      "reader beside them, suggesting a high-tech lock\n" ++
      "system. Everything in this room exudes a sense\n" ++
      "of being locked down, impenetrable, and designed\n" ++
      "to keep any potential threat contained.\n",

      interactables = [],
      items = [],
      north = Just lab,
      south = Just medium_security_prison,  -- Link back to medium_security_prison
      east = Just wrath_cell,
      west = Just generator_room
    }

generator_room :: Location
generator_room =
  Location
    { name = "Generator Room",
      description = "You are in the generator room.\n" ++
      "The room vibrates with the hum of a massive generator\n" ++
      "at its center, filling the air with the scent of\n" ++
      "oil and metal. Pipes and wires snake along the walls,\n" ++
      "some hissing softly. The floor is stained, and the\n" ++
      "flickering lights cast unsettling shadows, making\n" ++
      "the room feel both powerful and dangerous.\n",

      interactables = ["lit_torch", "generator"],
      items = [],
      north = Nothing,
      south = Nothing,
      east = Just high_security_prison,  -- Link back to high_security_prison
      west = Nothing
    }

wrath_cell :: Location
wrath_cell =
  Location
    { name = "Wrath Cell",
      description = "You are in the wrath cell.\n" ++
      "The cell is dark and cramped, with the walls\n" ++
      "seemingly closing in. Scratches and dents mar the\n" ++
      "surfaces, as if something—or someone—has been\n" ++
      "violently thrashing. In one corner, a broken chair\n" ++
      "lies half-smashed, and shattered glass glistens on\n" ++
      "the floor. The air is thick with a sense of rage that\n" ++
      "never left, a heaviness that hangs in the oppressive\n" ++
      "silence. A rusted, bloodstained chain dangles\n" ++
      "from the ceiling, suggesting past violence, while the\n" ++
      "faint smell of sweat and anger lingers in the air.\n",

      interactables = ["chain", "chair"],
      items = [],
      north = Nothing,
      south = Nothing,
      east = Nothing,
      west = Just high_security_prison  -- Link back to high_security_prison
    }


lab :: Location
lab =
  Location
    { name = "Laboratory",
      description = "Y0u ar3 1n th3 1ab0r@t0ry...\n" ++
      "Th3 d00r cl0s3s b3hind y0u...\n" ++
      "Gr33n l1qu1d f1lls th3 c0mp@rtm3nts w1th h@lf-c0nsci0us b0d13s.\n" ++
      "M@ny ch3m1c@l @pp@r@tus3s @r3 3v3rYwh3r3.\n" ++
      "@t th3 3nd, @ r3d c@rp3t c0v3rs th3 fl00r, w1th\n" ++
      "@ c1rcl3 0f fl@m3s @nd @n 0p3n b00k.\n\n" ++
      "P!CK:\n\n" ++
      "1. Y0u r34d wh@ts 1n th3 b00k @nd p3rf0rm th3 r1tual,\n" ++
      "b3c0m1ng 1mm0rt@l, but y0u c@nn0t 3v3r\n" ++
      "l3@v3 th1s h0us3 @g@1n.\n\n" ++
      "2. Y0u br34k th3 c0mp@rtm3nts @nd try t0 s@v3 th3s3 p30pl3,\n" ++
      "but y0u d0nt kn0w wh@t th3Y r34llY @r3.\n\n" ++
      "3. You noticed a chirping bird that flew out through a hole in the wall.\n" ++
      "You decide to escape through it too.\n",

      interactables = ["r1tual", "h3lp", "a1sl3"],
      items = [],
      north = Nothing,
      south = Nothing,  -- No way back to high_security_prison
      east = Nothing,
      west = Nothing
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
  let loc = location state
  in if "slingshot" `elem` interactables loc
       then
         if hasPlayedSlingshot (flags state)
           then do
             putStrLn "You have already played the slingshot game."
             return state
           else do
             newState <- playGuessingGame state
             let newLoc = (location newState) {interactables = filter (/= "slingshot") (interactables (location newState))}
             return newState {location = newLoc}
       else do
         putStrLn "There is no slingshot here to interact with."
         return state

interactWith "piece_of_paper" state =
  let loc = location state
  in if "piece_of_paper" `elem` interactables loc
       then do
         putStrLn ""
         putStrLn "I am a sequence where each number grows"
         putStrLn "By adding the two that came before it, it shows"
         putStrLn "Find the pair just under 200s crest"
         putStrLn "The penultimate and ultimate in my test"
         putStrLn ""
         return state
       else do
         putStrLn "There is no piece of paper here to interact with."
         return state

interactWith "window" state =
  let loc = location state
  in if "window" `elem` interactables loc
       then do
         let windowState = isWindowOpen (flags state)
         if windowState
           then putStrLn "You close the window. It becomes quiet."
           else putStrLn "You open the window. You can hear the wind blowing from outside."
         let updatedFlags = (flags state) {isWindowOpen = not windowState}
         return state {flags = updatedFlags}
       else do
         putStrLn "There is no window here to interact with."
         return state

interactWith "painting" state =
  let loc = location state
  in if "painting" `elem` interactables loc
       then do
         putStrLn "In the painting: A thick fog blankets a dark, twisted forest"
         putStrLn "Gnarled trees loom like dark silhouettes, their branches"
         putStrLn "reaching out like claws. In the distance, a decaying"
         putStrLn "mansion stands, its broken windows resembling"
         putStrLn "hollow eyes. The faint glow of a flickering lantern"
         putStrLn "casts an eerie light on a narrow, winding path"
         putStrLn "leading toward the mansion, swallowed by the thick darkness."
         return state
       else do
         putStrLn "There is no painting here to interact with."
         return state

interactWith "chandelier" state =
  let loc = location state
  in if "chandelier" `elem` interactables loc
       then do
         putStrLn "The broken chandelier swings, its shattered crystals casting"
         putStrLn "twisted shadows in the dim light – better not walk beneath it."
         return state
       else do
         putStrLn "There is no chandelier here to interact with."
         return state

interactWith "skeletons" state =
  let loc = location state
  in if "skeletons" `elem` interactables loc
       then do
         putStrLn ""
         putStrLn "Decayed skeletons are scattered around. Some sit in chairs,"
         putStrLn "others lie on the floor, their bones twisted and lifeless."
         putStrLn "Flickering light casts eerie shadows on their empty eye sockets."
         putStrLn ""
         return state
       else do
         putStrLn "There are no skeletons here to interact with."
         return state

interactWith "crystal_ball" state =
  let loc = location state
  in if "crystal_ball" `elem` interactables loc
       then do
         putStrLn "You picked up the crystal ball, but it slipped"
         putStrLn "from your hands and fell to the ground, shattering."
         putStrLn "Something strange fell out of it."
         let newItems = "rune2" : items (location state)  -- Dodajemy "rune2" do items w lokacji
         let newLoc = (location state) {items = newItems}  -- Aktualizujemy lokację
         return state {location = newLoc}
       else do
         putStrLn "There is no crystal ball here to interact with."
         return state

interactWith "floor" state =
  let loc = location state
  in if "floor" `elem` interactables loc
       then do
         putStrLn "There is such a mess in here..."
         return state
       else do
         putStrLn "There is no floor here to interact with."
         return state

-- Default case: Inform that the item is not interactable in the current location
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
