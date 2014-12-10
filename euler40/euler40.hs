import Data.Char

numbersOfLength :: Int -> Int
numbersOfLength n = 10^n - 10^(n - 1)

helper :: Int -> Int -> Int
helper len index
    | index > orderLength = helper (len + 1) (index - orderLength)
    | otherwise           = digitToInt $ (show number) !! (index - len * precedents) 
    where orderLength = len * numbersOfLength len
          number = precedents + 1 + sum (map numbersOfLength [1..(len - 1)])
          precedents = quot index len
          
champernownDigit :: Int -> Int
champernownDigit index = helper 1 (index - 1)

main = print . product . (map champernownDigit) $ map (10^) [0..6]
