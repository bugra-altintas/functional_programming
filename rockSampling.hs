data Cell = SpaceCraft Int | Sand | Rock Int | Pit deriving (Eq, Read, Show)

type Grid = [[Cell]]
type Coordinate = (Int, Int)

data Move = North | East | South | West | PickUp | PutDown deriving (Eq, Read, Show)

data Robot = Robot { name :: String,
                     location :: Coordinate,
                     capacity :: Int,
                     energy :: Int,
                     storage :: Int } deriving (Read, Show)


isInGrid :: Grid -> Coordinate -> Bool
isInGrid [] _ = False
isInGrid grid coor = ((fst coor) >= 0 && (fst coor) < (rowLen grid) && (snd coor) >= 0 && (snd coor) < (length grid))

rowLen grid = (foldl (+) 0 [length row | row<-grid]) `div` (length grid)
-------------------------------------------------------------------------------------------

totalCount :: Grid -> Int
totalCound [] = 0
totalCount grid = foldr (+) 0 [rocks row | row<-grid]

rocks row = foldr (+) 0 [fst (readCell cell) | cell<-row, (detectRock cell)]

detectRock (Rock _) = True
detectRock x = False

readCell (SpaceCraft num) = (num,"sp")
readCell (Rock num) = (num,"r")
readCell Sand = (-1,"s")
readCell Pit = (-9,"p")
-------------------------------------------------------------------------------------------

coordinatesOfPits :: Grid -> [Coordinate]
coordinatesOfPits grid = whereArePits grid (rowLen grid) 0

isPit [] _ _ = False
isPit (cell:row) x n | x == n = if (readCell cell) == (-9,"p") then True else False
                     | n < x = isPit row x (n+1)
                     | otherwise = False
colPits [] _ _ = []
colPits (row:grid) x y = (if (isPit row x 0)  then [(x,y)] else []) ++ (colPits grid x (y+1))

whereArePits [] _ _ = []
whereArePits grid rowl x | x == rowl = []
                         | otherwise = (colPits grid x 0) ++ (whereArePits grid rowl (x+1))

-------------------------------------------------------------------------------------------

tracePath :: Grid -> Robot -> [Move] -> [Coordinate]
tracePath [] _ _ = []
tracePath _ _ [] = []
tracePath grid robot (move:moves) = [location (moveRobot1 grid robot move (elem (location robot) (coordinatesOfPits grid)))] ++ (tracePath grid (moveRobot1 grid robot move (elem (location robot) (coordinatesOfPits grid))) moves)


moveRobot1 grid robot move isInPit | isInPit == True = Robot {name = (name robot), location = (location robot), capacity = (capacity robot), energy = if (energy robot) - (neededEnergy move) > 0 then (energy robot) - (neededEnergy move) else 0,storage = (storage robot)}
                                  | (energy robot) - (neededEnergy move) >= 0 = Robot {name = (name robot), location = (changeLocation grid (location robot) move), capacity = (capacity robot), energy = (energy robot)-(neededEnergy move),storage = changeStorage (storage robot) (capacity robot) move}
                                  | otherwise = Robot {name = (name robot), location = (location robot), capacity = (capacity robot), energy = 0,storage = (storage robot)}

changeLocation grid loc West = if (fst loc)-1 >=0 then ((fst loc)-1,snd loc) else loc
changeLocation grid loc East = if (fst loc)+1 < (rowLen grid) then ((fst loc)+1,snd loc) else loc
changeLocation grid loc North = if (snd loc)-1 >= 0 then (fst loc,(snd loc)-1) else loc
changeLocation grid loc South = if (snd loc)-1 < (length grid) then (fst loc,(snd loc)+1) else loc
changeLocation grid loc PickUp = loc
changeLocation grid loc PutDown = loc

changeStorage s c PickUp = if c>s then s+1 else s
changeStorage s _ PutDown = if s>0 then s-1 else s
changeStorage s c x = s

neededEnergy PickUp = 5
neededEnergy PutDown = 3
neededEnergy x = 1


energiseRobots :: Grid -> [Robot] -> [Robot]
energiseRobots grid robots = [giveEnergy robot (gain (whereIsSP_grid grid 0) (location robot)) | robot<-robots]

giveEnergy robot 0 = robot 
giveEnergy robot gain | (energy robot) + gain > 100 = Robot {name = (name robot), location = (location robot), capacity = (capacity robot), energy = 100,storage = (storage robot)}
                      | otherwise = Robot {name = (name robot), location = (location robot), capacity = (capacity robot), energy = (energy robot) + gain ,storage = (storage robot)}

gain loc_sp loc_robot = max 0 (100 - (abs (fst loc_robot - fst loc_sp) + abs (snd loc_robot - snd loc_sp)) * 20)

whereIsSP_grid (row:grid) y | whereIsSP_row row 0 y == (-3,-3) = whereIsSP_grid grid (y+1)
                       | otherwise = whereIsSP_row row 0 y

whereIsSP_row [] _ _ = (-3,-3)
whereIsSP_row (cell:row) x y | snd (readCell cell) == "sp" = (x,y)
                         | otherwise = whereIsSP_row row (x+1) y

-------------------------------------------------------------------------------------------

applyMoves :: Grid -> Robot -> [Move] -> (Grid, Robot)
applyMoves grid robot [] = (grid,robot)
applyMoves grid robot (move:moves) = applyMoves (fst (moveRobot2 grid robot move)) (snd ((moveRobot2 grid robot move))) moves

moveRobot2 grid robot PickUp | (energy robot)-(neededEnergy PickUp)>0 && (storage robot) < (capacity robot) = (updateGrid_grid grid (location robot) 0 PickUp , moveRobot1 grid robot PickUp False)
                             | otherwise = (grid, moveRobot1 grid robot PickUp False)
moveRobot2 grid robot PutDown | (energy robot)-(neededEnergy PutDown)>0 = (updateGrid_grid grid (location robot) 0 PutDown , moveRobot1 grid robot PutDown False)
                              | otherwise = (grid, moveRobot1 grid robot PutDown False)
moveRobot2 grid robot x = (grid, moveRobot1 grid robot x (elem (location robot) (coordinatesOfPits grid)))

updateGrid_grid [] _ _ _ = []
updateGrid_grid (row:grid) (rx,ry) y move | ry == y = (updateGrid_row row rx 0 move):grid
                                          | ry > y = row:(updateGrid_grid grid (rx,ry) (y+1) move)

updateGrid_row [] _ _ _  = []
updateGrid_row (cell:row) x n move | x == n = (updateGrid_cell cell move):row
                                   | n < x = cell:(updateGrid_row row x (n+1) move)


updateGrid_cell cell PickUp | fst (readCell cell) > 0 = (Rock (fst (readCell cell) -1))
                             | otherwise = cell
updateGrid_cell cell PutDown = (SpaceCraft (fst (readCell cell) +1))
updateGrid_cell cell _ = cell
