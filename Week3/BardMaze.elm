import System.Random

type Maze = Maze {
  cells : Array Array Array Int
  }

createMaze : Int -> Int -> Int -> Maze
createMaze width height depth =
  let
    randomCell = Random.randInt (0, width * height * depth)
    cells = Array.init (width * height * depth) (\i ->
      if i == randomCell then 1 else 0)
  in
    Maze cells

createRandomMaze : Int -> Int -> Int -> Maze
createRandomMaze width height depth =
  let
    maze = createMaze width height depth
    while not (isSolvable (maze)) do
      maze = createMaze width height depth
  in
    maze

isSolvable : Maze -> Bool
isSolvable maze =
  let
    start = (0, 0, 0)
    goal = (width - 1, height - 1, depth - 1)
    visited = Set.empty
  in
    dfs maze start visited goal

dfs : Maze -> (Int, Int, Int) -> Set (Int, Int, Int) -> (Int, Int, Int) -> Bool
dfs maze current visited goal =
  case current of
    (x, y, z) ->
      if x == goal.x && y == goal.y && z == goal.z then
        true
      else
        if visited.contains current then
          false
        else
          let
            neighbors =
              [(x + 1, y, z), (x - 1, y, z), (x, y + 1, z), (x, y - 1, z), (x, y, z + 1), (x, y, z - 1)]
            next =
              List.filter (\(nx, ny, nz) -> maze.cells.at nx. nz. ny == 1) neighbors
          in
            dfs maze (List.head next) (Set.insert current visited) goal
