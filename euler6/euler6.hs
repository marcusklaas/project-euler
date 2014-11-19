answer :: Integer
answer = (100+1) * (100+1) * 100 * 25 - sum [n*n | n <- [1..100]]

main = print (answer)
