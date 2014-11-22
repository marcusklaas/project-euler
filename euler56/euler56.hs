import Data.Char

main = print . maximum $ map digitSum [a^b | a <- [1..99], b <- [1..99]]
  where digitSum = sum . (map digitToInt) . show
