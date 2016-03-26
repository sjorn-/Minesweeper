module Main where

import Control.Monad.Random
import Control.Monad.State.Lazy
import System.Environment
import Data.List
import System.Console.ANSI

data Grid = Grid [[Char]]
type Move = (Char, (Int, Int))

instance Show Grid where
  show (Grid []) = "Game over."
  show (Grid xs) = "\n" ++ intercalate "\n" (map out (zip (map show [1..]) (map fix xs))) ++ "\n" ++ xNumbers ++ "\n"
    where
      out (n, ys) = spacePad 3 n ++ (concat ((map (\y -> take (seperationDistance) (repeat ' ')  ++ [y]) ys)))
      spacePad n x = take (((n-) . length) x) (repeat ' ') ++ (x)
      xNumbers = "    " ++ intercalate " " (map (\j -> spacePad seperationDistance (show j)) [1..length (head xs)])
      seperationDistance = (length . show . length) (head xs)
      fix = map (\x -> if x == '0' then ' ' else x)

main :: IO ()
main = do
  args <- getArgs
  setTitle "Minesweeper"
  clearScreen
  let (w, h, n) = getDimensions (args)
  mines <- (placeMines (w, h) n)
  -- putStrLn (show mines)
  x <- (gameLoop (w, h) mines [])--(startState w h))
  clearScreen
  putStrLn (show mines)
  if (x) then putStrLn ("You won!") else putStrLn ("You lose!")

getDimensions :: [[Char]] -> (Int, Int, Int)
getDimensions [x, n] = (read x, read x, read n)
getDimensions [x, y, n] = (read x, read y, read n)
getDimensions xs = error "Enter three digits"

gameLoop :: (Int, Int) -> Grid -> [Move] -> IO Bool
gameLoop size mines@(Grid xs) ms = do
  if ys == [] then return False
  else if finished then return True
  else do
    clearScreen
    putStrLn ((show . last) game)
    move <- getLine
    let moveType = words move
    let move = ((head . head) moveType, (read (moveType !! 2), read (moveType !! 1)))
    gameLoop size mines (ms ++ [move])
  where
    game = gameStep size mines ms
    (Grid ys) = last game
    finished = (sum (map (length . filter (\c -> c == 'F' || c == 'X')) ys)) == (sum (map (length . filter (\c -> c == 'M')) xs))

gameStep :: (Int, Int) -> Grid -> [Move] -> [Grid]
gameStep (w, h) mines@(Grid xs) ms = scanl f (createGrid (w, h)) ms
  where
    f uncovered (moveType, (x, y)) =
      case (moveType) of
        'F' -> flag uncovered
        'S' -> sweep uncovered
        _  -> uncovered
      where
        flag uncovered
          | chosen uncovered == 'X' = (set (x, y) 'F' uncovered)
          | chosen uncovered == 'F' = (set (x, y) 'X' uncovered)
          | otherwise = uncovered
        sweep uncovered
          | not flagged && mine == 'M' = (Grid [])
          | not flagged && mine /= '0' = (set (x, y) mine uncovered)
          | not flagged && otherwise = (checkSurrounding (x, y) (set (x, y) mine uncovered) mines)
          | otherwise = uncovered
          where flagged = chosen uncovered == 'F'
        mine = (xs !! (y - 1) !! (x - 1))
        chosen (Grid ys) = (ys !! (y - 1) !! (x - 1))

checkSurrounding :: (Int, Int) -> Grid -> Grid -> Grid
checkSurrounding (x, y) (Grid xs) mines@(Grid ys) = foldr f (Grid xs) [(a, b) | a <- [x-1..x+1], b <- [y-1..y+1], a /= x || b /= y, a > 0, b > 0, a <= length (head xs), b <= length xs]
  where
    f (a, b) k
      | (xs !! (b - 1) !! (a - 1)) == '0' = k
      | mine == '0' = checkSurrounding (a, b) (set (a, b) '0' k) mines
      | mine == 'M' = k
      | otherwise = (set (a, b) mine k)
      where
        mine = (ys !! (b - 1) !! (a - 1))

set :: (Int, Int) -> Char -> Grid -> Grid
set (x, y) c (Grid xs) = (Grid (as ++ [r'++(c:b')] ++ bs))
  where
    (as,r:bs) = splitAt (y - 1) xs
    (r',_:b') = splitAt (x - 1) r

minePositions :: (MonadRandom m) => (Int, Int) -> Int -> m [(Int, Int)]
minePositions (w, h) n = do
  xs <- getRandomRs(0, w - 1 :: Int)
  ys <- getRandomRs(0, h - 1 :: Int)
  return (take n (nub (zip xs ys)))

bundle :: Int -> [a] -> [[a]]
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
