smallestDivisor :: Int -> Int
smallestDivisor n = last (take 2 [ x | x <- [1..], n `mod` x == 0 ])

factorize :: Int -> [Int]
factorize 1 = []
factorize n = x:(factorize (n `div` x))
    where x = smallestDivisor n

answer = maximum (factorize 600851475143)

main = print (answer)
