-- Suffer within - game by Jakub Antas, Jan Górski, Patryk Zdziech


introductionText = [
    "A long time ago, in a galaxy far, far away...",
    "",
    "It is a period of civil war. Rebel",
    "spaceships, striking from a hidden",
    "base, have won their first victory",
    "against the evil Galactic Empire.",
    "",
    "During the battle, Rebel spies managed",
    "to steal secret plans to the Empire's",
    "ultimate weapon, the Death Star, an",
    "armored space station with enough",
    "power to destroy an entire planet.",
    "",
    "Pursued by the Empire's sinister agents,",
    "Princess Leia races home aboard her",
    "starship, custodian of the stolen plans",
    "that can save her people and restore",
    "freedom to the galaxy....",
    ""
    ]

instructionsText = [
    "Available commands are:",
    "",
    "instructions  -- to see these instructions.",
    "quit          -- to end the game and quit.",
    ""
    ]

-- print strings from list in separate lines
printLines :: [String] -> IO ()
printLines xs = putStr (unlines xs)
                  
printIntroduction = printLines introductionText
printInstructions = printLines instructionsText

readCommand :: IO String
readCommand = do
    putStr "> "
    xs <- getLine
    return xs

--------------------------------------------------------------------------------
-- Locations -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Definicja typu Location
data Location = Location { name :: String, north :: Maybe LocationWItems, south :: Maybe LocationWItems, east :: Maybe LocationWItems, west :: Maybe LocationWItems }
type Interactables = [String]
data LocationWItems = LocationWItems { location :: Location, interactables :: Interactables }

displayInteractables :: Interactables -> IO ()
displayInteractables xs = putStr (unlines ("Interactables:" : xs))


-- Funkcja do zmiany pokoju na podstawie kierunku
moveT :: LocationWItems -> Char -> LocationWItems
moveT currentLocation direction = do
    let loc = location currentLocation
    case direction of
        'n' -> maybe currentLocation id (north loc)
        's' -> maybe currentLocation id (south loc)
        'e'  -> maybe currentLocation id (east loc)
        'w'  -> maybe currentLocation id (west loc)
        _       -> currentLocation

-- Funkcja do wyświetlania pokoju
displayLocation :: Location -> IO ()
displayLocation location = putStrLn $ "You are in the " ++ name location

-- LOKALIZACJE
garden :: Location
garden = Location { name = "Garden", north = Just hallW, south = Nothing, east = Just glasshouseW, west = Nothing }
hall :: Location
hall = Location { name = "Hall", north = Nothing, south = Just gardenW, east = Just banquetHallW, west = Just libraryW }
glasshouse :: Location
glasshouse = Location { name = "Glasshouse", north = Nothing, south = Nothing, east = Nothing, west = Just gardenW }
library :: Location
library = Location { name = "Library", north = Nothing, south = Nothing, east = Just hallW, west = Nothing }
banquetHall :: Location
banquetHall = Location { name = "Banquet Hall", north = Nothing, south = Nothing, east = Just closetW, west = Just hallW }
closet :: Location
closet = Location { name = "Closet", north = Nothing, south = Nothing, east = Nothing, west = Just banquetHallW }

-- LOKALIZACJE Z ITEMAMY
gardenW :: LocationWItems
gardenW = LocationWItems { location = garden, interactables = [] }
hallW :: LocationWItems
hallW = LocationWItems { location = hall, interactables = [] }
glasshouseW :: LocationWItems
glasshouseW = LocationWItems { location = glasshouse, interactables = ["slingshot"] }
libraryW :: LocationWItems
libraryW = LocationWItems { location = library, interactables = ["piece_of_paper", "window", "bool"] }
banquetHallW :: LocationWItems
banquetHallW = LocationWItems { location = banquetHall, interactables = [ "painting", "chandelier", "skeletons" ] }
closetW :: LocationWItems
closetW = LocationWItems { location = closet, interactables = ["crystal ball", "floor"] }

moveRecursively :: LocationWItems -> IO ()
moveRecursively pos = do
    let position = location pos
    let int = interactables pos
    displayLocation position
    displayInteractables int
    putStr "Enter direction (n/s/e/w, or 'q' to quit): "
    command <- getLine
    case command of
        "q" -> putStrLn "Goodbye!"
        _ -> do
            moveRecursively (foldl moveT pos command)

gameLoop :: IO ()
gameLoop = do
    cmd <- readCommand
    case cmd of
        "instructions" -> do printInstructions
                             gameLoop
        "quit" -> return ()
        _ -> do printLines ["Unknown command.", ""]
                gameLoop

main = do
    printIntroduction
    printInstructions
    moveRecursively gardenW

