naiveSet = [(a, b, c) | a <- [1..333], b <- [1..999], c <- [333..999], a < b, b < c, a * a + b * b == c * c, a + b + c == 1000]

solution = head naiveSet

-- pattern matching! :-)

prodTriple :: (Num a) => (a, a, a) -> a
prodTriple (a, b, c) = a * b * c

prod = prodTriple solution

main = print prod
