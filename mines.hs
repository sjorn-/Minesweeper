module Main where

import Control.Monad
import Control.Monad.Random
import Control.Monad.State.Lazy
import System.Environment
import System.IO
import Data.List
import System.Console.ANSI

data Grid = Grid [[Char]]

instance Show Grid where
  show (Grid xs) = "\n" ++ firstNumbers ++ "\n" ++ intercalate "\n" (map out (zip (map show [1..]) (map fix xs))) ++ "\n"
    where
      out (n, ys) = spacePad 3 n ++ (concat ((map (\y -> take (seperationDistance) (repeat ' ')  ++ [y]) ys)))
      spacePad n x = take (((n-) . length) x) (repeat ' ') ++ (x)
      firstNumbers = "    " ++ intercalate " " (map (\j -> spacePad seperationDistance (show j)) [1..length (head xs)])
      seperationDistance = (length . show . length) (head xs)
      fix = map (\x -> if x == '0' then ' ' else x)

main = do
  args <- getArgs 
  setTitle "Minesweeper"
  clearScreen
  let (w, h) = getDimensions (args)
  mines <- (placeMines (w, h) 3)
  putStrLn (show mines)
  -- score <- gameLoop (createGrid (w, h)) mines
  x <- (gameLoop mines)-- (startState w h)
  putStrLn (show x)
  -- if (score) then putStrLn ("You won!") else putStrLn ("You lose!")
  
startState w h = (True, (createGrid (w, h)))
  
getDimensions [x] = (read x, read x) 
getDimensions [x, y] = (read x, read y)
getDimensions xs = error "Enter two digits"

  
splitStr _ [] = []  
splitStr x xs = if length str > 0 then str : splitStr x (drop (n+1) xs) else splitStr x (drop (n+1) xs) 
  where 
    n = length (takeWhile (/=x) xs)
    str = take n xs
    
-- gameLoop :: Grid -> Grid -> IO Int
-- -- gameLoop = do
-- gameLoop uncovered mines = do
--   -- clearScreen
--   putStrLn (show uncovered)
--   move <- getLine
--   let moveType = splitStr ' ' move
--   let x = read (moveType !! 1)
--   let y = read (moveType !! 2)
--   a <- case (head moveType) of
--     "F" -> (gameLoop (flag (x, y)) mines)
--     "S" -> (sweep (x, y))-- (gameLoop (sweep (x, y)) mines)
--   if finished then return 1 else return a
--   where
--     (Grid xs) = mines
--     (Grid ys) = uncovered
--     flag (x, y) = (set (x, y) 'F' uncovered)--(Grid (let (xs:ys) = splitAt y [] ))
--     sweep (x, y) = if mine == 'M' then return 0 else gameLoop (set (x, y) mine uncovered) mines
--       where
--         mine = (xs !! (y - 1) !! (x - 1))
--     finished = (sum (map (length . filter (\c -> c == 'F' || c == 'X')) ys)) == (sum (map (length . filter (\c -> c == 'M')) xs))

gameLoop :: Grid -> State (Bool, Grid) Bool
gameLoop mines@(Grid xs) = do
  -- clearScreen
  (score, uncovered) <- get
  if (finished uncovered) then do 
    put (True, uncovered)
    return True
  else if (not score) then do
    put (False, uncovered)
    return False
  else do
    -- putStrLn (show uncovered) <---------
    -- move <- getLine <---------
    let moveType = splitStr ' ' move
    let x = read (moveType !! 1)
    let y = read (moveType !! 2)
    -- if (head moveType) == "F" then
    --   put (score, (gameLoop (flag (x, y) uncovered) mines))
    -- else if (head moveType) == "S" then
    --   let this = (sweep (x, y) uncovered) in if this == 0 then put (False, uncovered) else put (True, this)
    -- else  error "Invalid"
    -- return 0
    case (head moveType) of
      "F" -> put (score, (flag (x, y) uncovered))
      "S" -> let this = (sweep (x, y) uncovered) in 
              if empty (this)
                then put (False, uncovered) 
                else put (True, this)  -- (gameLoop (sweep (x, y)) mines)
      --  _  -> error "Invalid Input"
    gameLoop mines
  where
    flag (x, y) uncovered = (set (x, y) 'F' uncovered)--(Grid (let (xs:ys) = splitAt y [] ))
    sweep (x, y) uncovered = if mine == 'M' then (Grid []) else (set (x, y) mine uncovered)
      where
        mine = (xs !! (y - 1) !! (x - 1))
    finished (Grid ys) = (sum (map (length . filter (\c -> c == 'F' || c == 'X')) ys)) == (sum (map (length . filter (\c -> c == 'M')) xs))
  
empty (Grid []) = True
empty xs = False
  
set (x, y) c (Grid xs) = (Grid (as ++ [r'++(c:b')] ++ bs))
  where
    (as,r:bs) = splitAt (y - 1) xs
    (r',_:b') = splitAt (x - 1) r
    

-- flag :: Grid -> (Int, Int) -> Grid
-- flag = undefined
  
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
