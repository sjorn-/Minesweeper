module Main where

import Control.Monad.Random
import Control.Monad.State.Lazy
import System.Environment
import Data.List
import System.Console.ANSI

data Grid = Grid [[Char]]

instance Show Grid where
  show (Grid xs) = "\n" ++ intercalate "\n" (map out (zip (map show [1..]) (map fix xs))) ++ "\n\n" ++ xNumbers ++ "\n"
    where
      out (n, ys) = spacePad 3 n ++ (concat ((map (\y -> take (seperationDistance) (repeat ' ')  ++ [y]) ys)))
      spacePad n x = take (((n-) . length) x) (repeat ' ') ++ (x)
      xNumbers = "    " ++ intercalate " " (map (\j -> spacePad seperationDistance (show j)) [1..length (head xs)])
      seperationDistance = (length . show . length) (head xs)
      fix = map (\x -> if x == '0' then ' ' else x)

main = do
  args <- getArgs
  setTitle "Minesweeper"
  clearScreen
  let (w, h, n) = getDimensions (args)
  mines <- (placeMines (w, h) n)
  putStrLn (show mines)
  x <- (gameLoop mines (startState w h))
  putStrLn (show mines)
  if (x) then putStrLn ("You won!") else putStrLn ("You lose!")

startState w h = (True, (createGrid (w, h)))

getDimensions [x, n] = (read x, read x, read n)
getDimensions [x, y, n] = (read x, read y, read n)
getDimensions xs = error "Enter three digits"

gameLoop mines@(Grid xs) state = do
  putStrLn (show (snd state))
  if (not (fst state)) then return False
  else if (finished) then return True
  else do
    move <- getLine
    let moveType = words move
    let x = read (moveType !! 1)
    let y = read (moveType !! 2)
    let newState = snd (runState (gameStep mines ((head . head) moveType, (x, y))) state)
    gameLoop mines newState
  where
    (Grid ys) = snd state
    finished = (sum (map (length . filter (\c -> c == 'F' || c == 'X')) ys)) == (sum (map (length . filter (\c -> c == 'M')) xs))

gameStep :: Grid -> (Char, (Int, Int)) -> State (Bool, Grid) ()
gameStep mines@(Grid xs) (moveType, (x, y)) = do
  (score, uncovered) <- get
  case (moveType) of
    'F' -> put (score, (flag uncovered))
    'S' -> let this = (sweep uncovered) in
            if empty (this)
              then put (False, uncovered)
              else put (True, this)
    _  -> put (score, uncovered)
  where
    flag uncovered = if chosen uncovered == 'X' then (set (x, y) 'F' uncovered) else do uncovered--(Grid (let (xs:ys) = splitAt y [] ))
    sweep uncovered = if chosen uncovered /= 'F' then
      if mine == 'M'
        then (Grid [])
        else if (chosen uncovered /= '0')
          then (set (x, y) mine uncovered)
          else (checkSurrounding (x, y) (set (x, y) mine uncovered) mines)
        else uncovered
    mine = (xs !! (y - 1) !! (x - 1))
    chosen (Grid ys) = (ys !! (y - 1) !! (x - 1))

empty (Grid []) = True
empty xs = False

checkSurrounding (x, y) (Grid xs) mines@(Grid ys) = foldr f (Grid xs) [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a /= x || b /= y, a > 0, b > 0, a <= length (head xs), b <= length xs]
  where
    f (a, b) k
      | (xs !! (b - 1) !! (a - 1)) == '0' = k
      | (ys !! (b - 1) !! (a - 1)) == '0' = checkSurrounding (a, b) (set (a, b) '0' k) mines
      | (ys !! (b - 1) !! (a - 1)) == 'M' = k
      | otherwise = (set (a, b) (ys !! (b - 1) !! (a - 1)) k)
  --map (\(a, b) -> if (xs !! (b - 1) !! (a - 1)) == '0' then checkSurrounding (a, b) (set (a, b) '0' (Grid xs)) else (set (a, b) (xs !! (b - 1) !! (a - 1)) (Grid xs))) [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a /= x || b /= y, a >= 0, b >= 0, a <= length (head xs), b <= length xs]

set (x, y) c (Grid xs) = (Grid (as ++ [r'++(c:b')] ++ bs))
  where
    (as,r:bs) = splitAt (y - 1) xs
    (r',_:b') = splitAt (x - 1) r

minePositions :: (MonadRandom m) => (Int, Int) -> Int -> m [(Int, Int)]
minePositions (w, h) n = do
  xs <- getRandomRs(0, w - 1 :: Int)
  ys <- getRandomRs(0, h - 1 :: Int)
  return (take n (nub (zip xs ys)))

bundle n [] = []
bundle n xs = take n xs : bundle n (drop n xs)

placeMines :: (MonadRandom m) => (Int, Int) -> Int -> m Grid
placeMines (w, h) n = do
  ps <- minePositions (w, h) n
  let grid = [c | y <- [0..h - 1], x <- [0..w - 1], let c = if (x,y) `elem` ps then 'M' else head (show (numAround ps x y))]
  return (Grid (bundle w grid))
    where
      numAround ps x y = length (intersect ps [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a /= x || b /= y, a >= 0, b >= 0, a <= w, b <= h])

createGrid :: (Int, Int) -> Grid
createGrid (m, n) = (Grid (take n (repeat (take m (repeat 'X')))))
