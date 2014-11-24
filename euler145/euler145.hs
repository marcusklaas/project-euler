reversiblesOfLength :: Int -> Int
reversiblesOfLength n
    | even n         = 1 * 20 * 30^(quot n 2 - 1)
    | n `mod` 4 == 3 = 5 * 20 * (25 * 20)^(quot n 4)
    | otherwise      = 0

main :: IO()
main = print . sum $ map reversiblesOfLength [1..9]
