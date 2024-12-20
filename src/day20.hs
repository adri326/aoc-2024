import System.IO
import Data.List
import Data.Maybe

data Maze = Maze {
    grid :: [[Bool]],
    width :: Int,
    height :: Int,
    start :: (Int, Int),
    end :: (Int, Int)
} deriving (Show)

parseMaze :: String -> Maybe Maze
parseMaze raw =
    let split = lines raw in
    let grid = map (map (/= '#')) split in

    findIndex (elem 'S') split >>= \start_y ->
    elemIndex 'S' (split !! start_y) >>= \start_x ->

    findIndex (elem 'E') split >>= \end_y ->
    elemIndex 'E' (split !! end_y) >>= \end_x ->

    Just Maze {
        grid = grid,
        start = (start_x, start_y),
        width = length (grid !! 0),
        height = length grid,
        end = (end_x, end_y)
    }

mazeGet :: Maze -> (Int, Int) -> Bool
mazeGet maze (x, y) = (x >= 0 && y >= 0 && x < width maze && y < height maze) && (grid maze !! y !! x)

add2 :: forall a. Integral a => (a, a) -> (a, a) -> (a, a)
add2 (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

neighbors :: Maze -> (Int, Int) -> [(Int, Int)]
neighbors maze pos =
    filter (mazeGet maze) $ map (add2 pos) [(0, 1), (0, -1), (-1, 0), (1, 0)]

mazeDfs_ :: Maze -> (Int, Int) -> [(Int, Int)] -> Maybe [(Int, Int)]
mazeDfs_ maze pos closed = if end maze == pos then Just (pos:closed) else
    listToMaybe
    $ mapMaybe (\neighbor -> mazeDfs_ maze neighbor (pos:closed))
    $ filter (`notElem` closed) (neighbors maze pos)

mazeDfs :: Maze -> [(Int, Int)]
mazeDfs maze = fromJust $ mazeDfs_ maze (start maze) []

makeDistMap :: Maze -> Int -> [(Int, Int)] -> [[Int]]
makeDistMap maze _ [] = map (map (const 0)) $ grid maze
makeDistMap maze d ((x, y) : rest) =
    let dist = makeDistMap maze (d + 1) rest in
    imap (\y2 row ->
        if y == y2 then
            imap (\x2 v -> if x == x2 then d else v) row
        else
            row
    ) dist

imap :: (Int -> a -> b) -> [a] -> [b]
imap cb list =
    snd $ foldl (\(n, r) x -> (n + 1, r ++ [cb n x])) (0, []) list

neighbors2 :: Maze -> Int -> (Int, Int) -> [(Int, Int)]
neighbors2 maze dist pos =
    filter (mazeGet maze)
    $ map (add2 pos)
    $ filter (\(dx, dy) -> abs dx + abs dy <= dist)
    $ [(-dist)..dist] >>= (\x -> map (x,) [(-dist)..dist])

countCheats :: Maze -> Int -> Int -> [[Int]] -> [(Int, Int)] -> Int
countCheats maze cutoff maxDist distMap [] = 0
countCheats maze cutoff maxDist distMap ((x, y) : rest) =
    let currentDist = distMap !! y !! x in
    let cheats = length (filter (\(x2, y2) ->
                let d = abs (x2 - x) + abs (y2 - y) in
                (distMap !! y2 !! x2) + d <= currentDist - cutoff
            ) $ neighbors2 maze maxDist (x, y)) in
    cheats + countCheats maze cutoff maxDist distMap rest

main = do
    handle <- openFile "./input/day20.txt" ReadMode
    contents <- hGetContents handle
    let part1 = do
            maze <- parseMaze contents
            let path = mazeDfs maze
            let distMap = makeDistMap maze 0 path
            return (countCheats maze 100 2 distMap path)
    print $ fromJust part1
    let part2 = do
            maze <- parseMaze contents
            let path = mazeDfs maze
            let distMap = makeDistMap maze 0 path
            return (countCheats maze 100 20 distMap path)
    print $ fromJust part2
    hClose handle
