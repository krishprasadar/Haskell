module sudokuSolver where
import Data.List
import Solver

data SudokuConfig = SudokuConfig [Integer] deriving (Eq)

sudokuConfigFromList :: [Integer] -> SudokuConfig
sudokuConfigFromList x = SudokuConfig x

listFromSudokuConfig :: SudokuConfig -> [Integer]				
listFromSudokuConfig (SudokuConfig x) = x

instance Show SudokuConfig where
   show (SudokuConfig y) = sudoPrint y 0
          where sudoPrint [] _   = ""
                sudoPrint block 3 = "\n" ++ sudoPrint block 0
                sudoPrint block n = (row (take 9 block) "\n" 0) ++ (sudoPrint (drop 9 block) (n + 1))
                row [] newLine _     = newLine ++ " "
                row x  newLine 3     = row x (newLine ++ " ") 0 
                row (x:xs) newLine n = case x of 
                                    0 -> row xs (newLine ++ ("_") ++ " ") (n + 1) 
                                    otherwise -> row xs (newLine ++ (show x) ++ " ") (n + 1)  

getRow :: [Integer] -> Int -> [Integer]
getRow x n = row x y 0
  where  y= (n*9)                
         row [] _ _ = []
         row _ _ 9 = []         
         row x y z = [x!!y] ++ (row x (y + 1) (z+1))
--getRow trivial 8

rowSat::[Integer] -> Bool
rowSat x = (filter (/=[1..9]) (rowAll x)) == []
            where rowAll x = [sort (getRow x n) | n<- [0..8]]
--rowSat trivial

getCol :: [Integer] -> Int -> [Integer]
getCol x n = col x n 0
  where  col _ _ 9 = []
         col x n z = [x!!n] ++ (col x (n+9) (z+1))
--getCol trivial 5

colSat::[Integer] -> Bool
colSat x = (filter (/=[1..9]) (colSat x)) == []
            where colSat x = [sort (getCol x n) | n<- [0..8]]
--colSat trivial

getBlock :: [Integer] -> Int -> [Integer]
getBlock x y = block x y 0 0
   where block [] _ _ _ = []
         block _ _ _ 9  = []
         block x y 3 z  = (block x (y+6) 0 z)
         block x y u z  = [x!!y] ++ (block x (y+1) (u+1) (z+1))
--getBlock trivial 6

blockSat :: [Integer] -> Bool
blockSat x = (filter (/=[1..9]) (blockAll x)) == []
            where blockAll x = [sort (getBlock x n) | n<- [0,3,6,27,30,33,54,57,60]]
--blockSat trivial

getRowFromIndex :: [Integer] -> Int -> [Integer]
getRowFromIndex x n = getRow x (n `div` 9) 

--getRowFromIndex trivial 20

getColFromIndex :: [Integer] -> Int -> [Integer]
getColFromIndex x n = getCol x (((n `mod` 9) `div` 9)+(n`mod`9))
--getColFromIndex trivial 20

getBlockFromIndex :: [Integer] -> Int -> [Integer]
getBlockFromIndex x n = getBlock x (fetchBlockLeftTopMostFromIndex n [0,1,2,9,10,11,18,19,20])
--getBlockFromIndex trivial 20

fetchBlockLeftTopMostFromIndex :: Int -> [Int] -> Int
fetchBlockLeftTopMostFromIndex n (x:xs) = if ((n-x)`elem`[0,3,6,27,30,33,54,57,60]) then (n-x) else fetchBlockLeftTopMostFromIndex n xs
--fetchBlockLeftTopMostFromIndex 20 [0,1,2,9,10,11,18,19,20]

sudIsGoal :: SudokuConfig -> Bool
sudIsGoal (SudokuConfig x) = (rowSat x) && (colSat x) && (blockSat x)
--sudIsGoal (SudokuConfig trivial)

findPossiblesForCell :: [Integer] -> Int -> [Integer] 
findPossiblesForCell x n = [1..9] \\ (sort $ nub $ (getRowFromIndex x n) ++ (getColFromIndex x n) ++ (getBlockFromIndex x n)) 
--findPossiblesForCell trivial 0

instance Config SudokuConfig where
  successors (SudokuConfig []) = [] 
  successors (SudokuConfig x) = [SudokuConfig (replaceAtIndex x y z) | y <- (findPossiblesForCell x z) ]
   where z = findZero x 0

findZero :: [Integer]-> Int ->Int
findZero (0:[]) 80 = 80
findZero (_:[]) 80 = error ("Sudoku not solvable")
findZero (0:xs) y = y
findZero (_:xs) y = findZero xs (y+1)  
--findZero trivial 0

replaceAtIndex :: [Integer] -> Integer -> Int -> [Integer]
replaceAtIndex x y n = (take (fromIntegral n) x) ++  (y:drop (fromIntegral (n+1)) x)
--findZero trivial 0

sudoSolve :: SudokuConfig -> (Maybe SudokuConfig)
sudoSolve x = 
  let isGoal     = (sudIsGoal)
      initConfig = (x)
  in solve isGoal initConfig
