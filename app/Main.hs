import Data.List.Split
import Data.List(sort)

main :: IO ()
main = day1

day1:: IO ()
day1 = do contents <- readFile "data/1.txt"
          putStrLn "---------------"
          putStrLn  "Day 1: \n "
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



