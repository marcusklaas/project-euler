-- naive approach

divisibleByFirst :: Int -> Int -> Bool
divisibleByFirst 1 n = True
divisibleByFirst m n = n `mod` m == 0 && divisibleByFirst (m-1) n

answer = head (filter (divisibleByFirst 20) [1..])

main = print answer
