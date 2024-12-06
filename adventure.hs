-- The germ of a text adventure game
-- Marcin Szlenk 2024

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
data Location = Location { name :: String, north :: Maybe Location, south :: Maybe Location, east :: Maybe Location, west :: Maybe Location }

-- Funkcja do zmiany pokoju na podstawie kierunku
moveT :: Location -> Char -> Location
moveT currentLocation direction = case direction of
    'n' -> maybe currentLocation id (north currentLocation)
    's' -> maybe currentLocation id (south currentLocation)
    'e'  -> maybe currentLocation id (east currentLocation)
    'w'  -> maybe currentLocation id (west currentLocation)
    _       -> currentLocation

-- Funkcja do wyświetlania pokoju
displayLocation :: Location -> IO ()
displayLocation location = putStrLn $ "You are in the " ++ name location

-- LOKALIZACJE
garden :: Location
garden = Location { name = "Garden", north = Just hall, south = Nothing, east = Just glasshouse, west = Nothing }
hall ::Location 
hall = Location { name = "Hall", north = Nothing, south = Just garden, east = Just banquetHall, west = Just library }
glasshouse ::Location 
glasshouse = Location { name = "Glasshouse", north = Nothing, south = Nothing, east = Nothing, west = Just garden }
library ::Location 
library = Location { name = "Library", north = Nothing, south = Nothing, east = Just hall, west = Nothing }
banquetHall ::Location 
banquetHall = Location { name = "Banquet Hall", north = Nothing, south = Nothing, east = Just closet, west = Just hall }
closet ::Location 
closet = Location { name = "Closet", north = Nothing, south = Nothing, east = Nothing, west = Just banquetHall }


moveRecursively :: Location -> IO ()
moveRecursively pos = do
    displayLocation pos
    putStr "Enter direction (n/s/e/w, or 'q' to quit): "
    command <- getLine
    case command of
        "q" -> putStrLn "Goodbye!"
        _ -> do moveRecursively (foldl moveT pos command)

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
    moveRecursively garden

