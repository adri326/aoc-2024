import Data.List
import Data.Maybe
import Debug.Trace (trace)
import System.IO
import Data.Function (fix)
import Data.Function.Memoize (memoFix)


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
    rec_lengths = zipWith (countControlMoves (depth - 1)) ('A' : moves) (moves ++ "A")

countControlMoves :: Int -> Char -> Char -> Int
countControlMoves _depth _fromChar _toChar = memoFix (\self args ->
    let (depth, fromChar, toChar) = args in
    let moves = getMoves controlKeypad fromChar toChar in
    let rec_lengths = zipWith (curry3 self (depth - 1)) ('A' : moves) (moves ++ "A") in
    if depth == 0 then
      0
    else
      length moves + sum rec_lengths
  ) (_depth, _fromChar, _toChar)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 cb x y z = cb (x, y, z)

solve depth code =
  (length code + sum lengths) * codeValue
  where
    codeValue = (read $ take (length code - 1) code) :: Int
    lengths = zipWith (countMoves mainKeypad depth) ('A' : code) code

main = do
  handle <- openFile "./input/day21.txt" ReadMode
  contents <- hGetContents handle
  putStr "Part 1: "
  print $ sum $ map (solve 3) $ lines contents
  putStr "Part 2: "
  print $ sum $ map (solve 26) $ lines contents
  hClose handle
