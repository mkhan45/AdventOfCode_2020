import System.IO
import Control.Monad
import Data.List
import Data.Maybe

-- https://stackoverflow.com/a/55743500
-- pretty  cool
rotate :: Int -> [a] -> [a]
rotate = drop <> take

-- finds numbers in the first argument which sum to the 2nd argument
-- permutation_matrix looks like [[1, 2, 3], [2, 3, 1], [3, 1, 2], ...]
-- (map =<< zip . head) :: [[a]] => [[a, a]]
-- map =<< zip . head $ mat = map (zip (head ls)) ls = map (zip . head $ mat) $ mat
findNums :: [Int] -> Int -> (Int, Int)
findNums ls target = 
    let permutation_matrix = iterate (rotate 1) ls
        zipped_perms = concat $ map =<< zip . head $ permutation_matrix
        predicate = (==2020) . uncurry (+)
    in
        fromJust $ find predicate zipped_perms

main = do
    file <- openFile "input.txt" ReadMode
    contents <- hGetContents file
    let list = map read (lines contents) :: [Int]
    let (a, b) = findNums list 2020
    print (a * b)
    hClose file
