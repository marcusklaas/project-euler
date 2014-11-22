choose n k = product [(n-k+1)..n] `quot` product [1..k]

main = print . length $ filter (> 10^6) [choose n k | n <- [1..100], k <- [1..100], k <= n]
