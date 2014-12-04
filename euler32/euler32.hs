import Data.List

productPandigital :: Int -> Int -> Bool
productPandigital x y = "123456789" == (sort . concat $ map show [x, y, x * y])

multiplierSet :: Int -> [Int]
multiplierSet n = filter (productPandigital n) [1..end]
    where end = min n $ quot (10^5) n

main = print . sum $ nub [x * y | x <- [1..10^4], y <- multiplierSet x]
