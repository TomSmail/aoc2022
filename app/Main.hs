module Main where
import Data.List.Split
import Data.List(sort)

main :: IO ()
main = do contents <- readFile "1.txt"
          let stringArray = splitContent contents
          let intArray = map (map (\x -> read x :: Int) ) stringArray 
          let sumArray = map sum intArray
          print "Solution 1"
          print $ foldl (\x y -> if x < y then y else x ) 0 sumArray
          print "Solution 2"
          print $ sum $ take 3 $ reverse $ sort sumArray


splitContent:: String -> [[String]]
splitContent xs 
    = splitWhen (== "") $ lines xs

-- splitWhen:: (a -> Bool) -> [a] -> [[a]]
-- splitWhen predicate xs = helper xs []
--     where helper [] holder = [holder]
--           helper (y:ys) holder 
--             | predicate y = helper ys (y : holder)
--             | otherwise = helper ys
