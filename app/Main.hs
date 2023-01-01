import Data.List.Split
import Data.List(sort,nub, transpose)
import Data.Char
import Text.XML.HXT.DOM.Util
import qualified Data.Text as T (replace, pack, splitOn)


main :: IO ()
main = day8

day1:: IO ()
day1 = do contents <- readFile "data/1.txt"
          putStrLn "---------------"
          putStrLn  "Day 1: "
          let stringArray = splitContent contents
          let intArray = map (map (\x -> read x :: Int) ) stringArray
          let sumArray = map sum intArray
          putStrLn "\n Solution 1: "
          print $ foldl (\x y -> if x < y then y else x ) 0 sumArray
          putStrLn "\n Solution 2: "
          print $ sum $ take 3 $ reverse $ sort sumArray
          putStrLn "---------------"

          where splitContent:: String -> [[String]]
                splitContent xs = splitWhen (== "") $ lines xs

day2:: IO ()
day2 = do contents <- readFile "data/2.txt"
          putStrLn "---------------"
          putStrLn  "Day 2:"
          let rpsArray = map words (lines contents)
          putStrLn "\n Solution 1: "
          print $ foldl (\x y -> x + grade y + (mark $ last y) ) 0 rpsArray
          putStrLn "\n Solution 2: "
          print $ foldl (\x y -> x + grade' y  ) 0 rpsArray
          putStrLn "---------------"
          where mark:: String -> Int
                mark z
                    | z == "X" = 1
                    | z == "Y" = 2
                    | otherwise = 3
                grade:: [String] -> Int
                grade [] = 0
                grade (x:b)
                    | x == "A" = if y == "X" then 3 else
                         if y == "Y" then 6 else 0
                    | x == "B" = if y == "X" then 0 else
                         if y == "Y" then 3 else 6
                    | otherwise = if y == "X" then 6 else
                         if y == "Y" then 0 else 3
                 where y = head b
                grade' [] = 0
                grade' (a:b)
                    | y == "X" = if a == "A" then mark' "C" else
                         if a == "B" then mark' "A" else mark' "B"
                    | y == "Y" = if a == "A" then 3 + mark' "A" else
                         if a == "B" then 3 +  mark' "B" else 3 + mark' "C"
                    | otherwise = if a == "A" then 6 + mark' "B" else
                         if a == "B" then 6 +  mark' "C" else 6 + mark' "A"
                  where y = head b
                mark':: String -> Int
                mark' z
                    | z == "A" = 1
                    | z == "B" = 2
                    | otherwise = 3

day3:: IO()
day3 = do contents <- readFile "data/3.txt"
          putStrLn "---------------"
          putStrLn  "Day 3:"
          let containers
               =  map (\xs -> splitAt (length xs `div` 2)  xs) $ lines contents
          let letters
               = map (\xs -> filter (\x -> x `elem` snd xs) (fst xs)) containers
          let answer
               =  sum $ map (\xs -> if isUpper (head xs) then ord (head xs) - 38 else ord (head xs) - 96) letters
          putStrLn "\n Solution 1: "
          print answer
          putStrLn "\n Solution 2: "
          let groups
               = chunksOf 3 $ lines contents
          let badges
               = map (\xs -> filter (\x -> x `elem` head (tail xs) && x `elem` head (tail $ tail xs) ) (head xs)) groups
          let answer2
               =  sum $ map (\xs -> if isUpper (head xs) then ord (head xs) - 38 else ord (head xs) - 96) badges
          print answer2
          putStrLn "---------------"

day4:: IO()
day4 = do contents <- readFile "data/4.txt"
          putStrLn "---------------"
          putStrLn  "Day 4:"
          let pairs
                = map (\x -> map (\(a:b:_) -> [decimalStringToInt a .. decimalStringToInt b]) x) $ map (\x -> map (splitOn "-") x) $ map (splitOn ",") (lines contents)
          let checks
                = map (\(x:y:_) -> (foldl (\prev z -> prev && z `elem` y ) True x) || (foldl (\prev z -> prev && z `elem` x ) True y) ) pairs
          let answer
                = foldl (\prev x -> if x then prev + 1 else prev) 0 checks
          putStrLn "\n Solution 1: "
          print answer
          let checks2
                = map (\(x:y:_) -> (foldl (\prev z -> prev || z `elem` y ) False x) && (foldl (\prev z -> prev || z `elem` x ) False y) ) pairs
          let answer2
                = foldl (\prev x -> if x then prev + 1 else prev) 0 checks2
          putStrLn "\n Solution 2: "
          print answer2
          putStrLn "---------------"

day5:: IO()
day5 = do contents <- readFile "data/5.txt"
          putStrLn "---------------"
          putStrLn  "Day 5:"
          let boardMoves
                = splitOn [""] $ lines contents
          let board
                = map (T.splitOn (T.pack "[ ]")) $ init $ map (\x -> T.replace (T.pack "    ") (T.pack "[ ] ") (T.pack x))  $ head boardMoves
          print board

day6:: IO()
day6 = do contents <- readFile "data/6.txt"
          putStrLn "---------------"
          putStrLn  "Day 6:"
          let intList p
                = foldl (\(num, prev) x  ->
                    if length (nub prev) /= length prev
                        then ((head num + 1) : num, tail prev ++ [x])
                     else ([head num + 1] ++ [0] ++ num, tail prev ++ [x]) )
                       ([0], take p contents) (drop p contents)
          let answer p
                = p + ( last . head . splitWhen (==0)
                . tail . reverse $ fst (intList p) )
          putStrLn "\n Solution 1: "
          print $ answer 4
          putStrLn "\n Solution 2: "
          print $ answer 14
          putStrLn "---------------"

day7:: IO()
day7 = do contents <- readFile "data/7.txt"
          putStrLn "---------------"
          putStrLn  "Day 7:"


day8:: IO()
day8 = do contents <- readFile "data/8.txt"
          putStrLn "---------------"
          putStrLn  "Day 8:"
        --   need 4x list, built from seeming if rows / columns of trees
        --  have hidden trees in them. This can be done with 2 folds 
          let rows = lines contents
          let columns = transpose rows
          let forwardRows
                = foldl (\(rowNum, coordList) row
                -> (rowNum + 1, coordList ++ (map (\(_, _, indexes) 
                -> map (\index -> (rowNum, index)) indexes) 
                $ foldl (\(colNum, remainingHeights, indexes) val
                -> if toInteger val `elem` remainingHeights
                    then (colNum + 1, [toInteger val..9] ,val ++ indexes)
                    else  (colNum + 1, remainingHeights, indexes))
                     (0, [0..9], []) row ) (0, []) ))
          print rows
          putStrLn "---------------"
          print columns
          putStrLn "---------------"