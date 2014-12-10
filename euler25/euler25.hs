import Data.List

fibonaci :: [Integer]
fibonaci = map fib [0..]
    where fib 0 = 1
          fib 1 = 1
          fib n = fibonaci !! (n - 1) + fibonaci !! (n - 2)

main = print $ do
    index <- findIndex (>= 10^999) fibonaci
    return $ index + 1
