import Base

-- ====================
-- Chunk 1
-- ====================

-- 1)
opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

-- 2)
noActions :: Item -> GameState -> Next GameState
noActions i _ = Same ("There is nothing that you can do with a " ++ show i ++ ".")

-- 3)
winRoom :: Room
winRoom = Room "the Secret's Chamber of Curry (SCC)" "a chamber full of ghee" True (Just Key) [] [] [] noActions

-- 4)
startRoom :: Room
startRoom = Room "the void" "an absolutely empty and dark place" False Nothing [(Spoon, "Floating in front of you")] [] [(East, winRoom), (North, hall)] noActions

-- 5)
hall :: Room
hall = Room "hall" "a 15th century castle's hall" False Nothing [] [WoodTroll 10 Key] [] checkIfMonsters

checkIfMonsters :: Item -> GameState -> Next GameState
checkIfMonsters Spoon (GS p r) =
  case monsters r of
    [] -> Same "There are no monsters to attack in this room."
    (m : ms) ->
      if health m <= 5
        then Progress "You killed the wood troll. It seems like it dropped a key!" (GS p r {monsters = ms, items = (holding m, "On the floor") : items r})
        else
          let hp = health m
           in Progress "You dealt 5 damages to the wood troll." (GS p (r {monsters = m {health = hp - 5} : ms}))

-- 6)
dan :: Player
dan = Player "Daniel" []

game0 :: GameState
game0 = GS dan startRoom

-- ====================
-- Chunk 2
-- ====================
--7)
instance Parsable Item where
  parse "key" = Just Key
  parse "spoon" = Just Spoon
  parse _ = Nothing

--8)
instance Parsable Direction where
  parse "north" = Just North
  parse "south" = Just South
  parse "east" = Just East
  parse "west" = Just West
  parse _ = Nothing

--9)
instance Parsable Command where
  -- ## QUESTION 17, CHUNK 3 => ADDED 'help' command
  -- *Base.hs changes at line 70
  --check if command is help
  parse "help" = Just Help
  --check if end
  parse "end" = Just End
  --check if command is grab
  parse ('g' : 'r' : 'a' : 'b' : _ : xs) =
    case parse xs of
      Nothing -> Nothing
      Just el -> Just (PickUp el)
  --check if command is use
  parse ('u' : 's' : 'e' : _ : xs) =
    case parse xs of
      Nothing -> Nothing
      Just el -> Just (Use el)
  --check if command is go
  parse ('g' : 'o' : _ : xs) =
    case parse xs of
      Nothing -> Nothing
      Just el -> Just (Move el)
  --if none of these strings is identified, then the command is badly typed
  parse _ = Nothing

--10)
tellResponse :: String -> IO ()
tellResponse s = putStrLn $ "<  " ++ s ++ "."

--11)
readCommand :: IO (Maybe Command)
readCommand = do
  putStr ">  "
  inp <- getLine
  return (parse inp)

-- ====================
-- Chunk 3
-- ====================

--12)
deleteFrom :: Eq a => a -> [(a, b)] -> [(a, b)]
deleteFrom _ [] = []
-- recursively check each element for id equivalency
deleteFrom del ((id, val) : xs)
  | id == del = xs -- if found, skip it and return the rest of the list
  | otherwise = (id, val) : deleteFrom del xs

--13)
leaveRoom :: Room -> Direction -> Room -> Room
leaveRoom fromRoom dir toRoom =
  let delDoors = deleteFrom (opposite dir) (doors toRoom)
   in toRoom {doors = (opposite dir, fromRoom) : delDoors}

--14)
step :: Command -> GameState -> Next GameState
step comm (GS p r) = case comm of
  Move dir ->
    case lookup dir (doors r) of
      Nothing -> Same "There is no door in that direction! =("
      Just toRoom ->
        if canEnterRoom p toRoom
          then Progress "You are entering another room!" (GS p (leaveRoom r dir toRoom))
          else Same "This room requires a special item to be accessed! You are not able to open the door."
        where
          canEnterRoom :: Player -> Room -> Bool
          -- checks if the player can enter the specified room
          canEnterRoom (Player _ inv) (Room _ _ _ req _ _ _ _) =
            case req of
              Nothing -> True
              Just i -> i `elem` inv
  PickUp it ->
    case lookup it (items r) of
      Nothing -> Same "This room does not contain that item! =("
      Just _ -> Progress "You retrieved the item." (GS p {inventory = it : inventory p} r {items = deleteFrom it (items r)})
  Use it ->
    if it `elem` inventory p
      then actions r it (GS p r)
      else Same "You do not have that item."

--15)
play :: GameState -> IO ()
play gs = do
  tellContext gs
  playLoop gs

playLoop :: GameState -> IO ()
playLoop (GS p r)
  | isWinRoom r = tellResponse "You win!"
  | otherwise = do
    command <- readCommand
    case command of
      Nothing -> do
        tellResponse "This is not a command"
        playLoop (GS p r)
      Just End -> tellResponse "Game ends"
      -- ## QUESTION 17, CHUNK 3 => ADDED 'help' command
      -- *Base.hs changes at line 70
      Just Help -> do
        tellResponse "Help menu"
        putStrLn "===========================================================================================================\n"
        putStrLn " Available commands:"
        putStrLn "   + help: shows this menu."
        putStrLn "   + go [direction]: go towards one of the available directions in the room."
        putStrLn "   + grab [item]: grab an item from the room."
        putStrLn "   + use [item]: use an item from your inventory."
        putStrLn "   + end: stops the game. \n"
        putStrLn " Where there is a definition in between '[element]', it has to be substituted with an appropriate element."
        putStrLn " e.g. [direction] ==> north"
        putStrLn "===========================================================================================================\n"
        putStr "Let me remind you..."
        tellContext (GS p r)
        playLoop (GS p r)
      Just c ->
        case step c (GS p r) of
          (Same message) -> do
            putStrLn message
            playLoop (GS p r)
          (Progress message nextGS) -> do
            putStrLn message
            tellContext nextGS
            playLoop nextGS

--16)
main :: IO ()
main = play game0