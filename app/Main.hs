import Data.List.Split
import Data.List(sort)

main :: IO ()
main = day2

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
                    | x == "A" = if y == "X" then 3 else if y == "Y" then 6 else 0
                    | x == "B" = if y == "X" then 0 else if y == "Y" then 3 else 6 
                    | otherwise = if y == "X" then 6 else if y == "Y" then 0 else 3 
                 where y = head b
                grade' [] = 0
                grade' (a:b) 
                    | y == "X" = if a == "A" then mark' "C" else if a == "B" then mark' "A" else mark' "B"
                    | y == "Y" = if a == "A" then 3 + mark' "A" else if a == "B" then 3 +  mark' "B" else 3 + mark' "C"
                    | otherwise = if a == "A" then 6 + mark' "B" else if a == "B" then 6 +  mark' "C" else 6 + mark' "A"
                  where y = head b
                mark':: String -> Int
                mark' z 
                    | z == "A" = 1
                    | z == "B" = 2
                    | otherwise = 3
                 
                
                        


