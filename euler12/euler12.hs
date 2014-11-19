divisorFunc :: Int -> Int
divisorFunc n = length [ 1 | x <- [1..n], n `mod` x == 0]

triangleNumber :: Int -> Int
triangleNumber 1 = 1
triangleNumber n = n + triangleNumber (n - 1)

triangles = [triangleNumber n | n <- [1..]]

answer = head [x | x <- triangles, divisorFunc x > 499]

main = print (answer)
