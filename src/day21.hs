import Data.List
import Data.Maybe
import Debug.Trace (trace)
import System.IO


mainKeypad = [
    "789",
    "456",
    "123",
    " 0A"
  ]

controlKeypad = [
    " ^A",
    "<v>"
  ]

order = "<v^>"
cmpOrder a b = compare (map (`elemIndex` order) a) (map (`elemIndex` order) b)

getPos keypad char =
  let row = fromMaybe 0 $ findIndex (elem char) keypad in
  (fromMaybe 0 $ elemIndex char (keypad !! row), row)

moveAny posChar negChar amount =
  if amount >= 0 then
    replicate amount posChar
  else
    replicate (-amount) negChar

moveUpDown = moveAny 'v' '^'
moveLeftRight = moveAny '>' '<'

getMoves keypad fromChar toChar =
  head $ filter (
    fst . foldl (\(okay, pos) current->
      if okay then
        let newPos = makeMove pos current in
          (keypad !! snd newPos !! fst newPos /= ' ', newPos)
      else
        (False, pos)
    ) (True, fromPos)
  ) candidates
  where
    fromPos = getPos keypad fromChar
    toPos = getPos keypad toChar
    leftRight = moveLeftRight $ fst toPos - fst fromPos
    upDown = moveUpDown $ snd toPos - snd fromPos
    candidates = sortBy cmpOrder [upDown ++ leftRight, leftRight ++ upDown]

makeMove :: (Int, Int) -> Char -> (Int, Int)
makeMove (x, y) '^' = (x, y - 1)
makeMove (x, y) '<' = (x - 1, y)
makeMove (x, y) '>' = (x + 1, y)
makeMove (x, y) 'v' = (x, y + 1)

countMoves :: [[Char]] -> Int -> Char -> Char -> Int
countMoves keypad 0 fromChar toChar = 0
countMoves keypad depth fromChar toChar =
  length moves + sum rec_lengths
  where
    moves = getMoves keypad fromChar toChar
    rec_lengths = zipWith (countMoves controlKeypad (depth - 1)) ('A' : moves) (moves ++ "A")

solve depth code =
  (length code + sum lengths) * codeValue
  where
    codeValue :: Int = read $ take (length code - 1) code
    lengths = zipWith (countMoves mainKeypad depth) ('A' : code) code

main = do
  handle <- openFile "./input/day21.txt" ReadMode
  contents <- hGetContents handle
  print $ sum $ map (solve 3) $ lines contents
  -- print $ sum $ map (solve 26) $ lines contents
  hClose handle
