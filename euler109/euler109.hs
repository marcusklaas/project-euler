data Throw = Miss | Single Int | Double Int | Tripple Int deriving (Eq, Ord)

checkOutNumbers = 25:[1..20]

trippleThrows  = map Tripple [1..20] 
checkOutThrows = map Double checkOutNumbers
allThrows      = [Miss] ++ (map Single checkOutNumbers) ++ checkOutThrows ++ trippleThrows

score :: Throw -> Int
score Miss        = 0
score (Single n)  = n
score (Double n)  = 2 * n
score (Tripple n) = 3 * n

preceedingThrows :: Throw -> [(Throw, Throw)]
preceedingThrows t3 = [(t2, t1) | t2 <- allThrows, t1 <- allThrows, t1 <= t2, (sum $ map score [t3, t2, t1]) < 100]

main = print . sum $ map (length . preceedingThrows) checkOutThrows
