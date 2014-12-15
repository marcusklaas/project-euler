split :: Integer -> Integer -> [[Integer]]
split 1 n
    | n > 10    = []
    | otherwise = [[n]]
split m n = [x : rest | x <- candidates, rest <- split (m - 1) (n - x)]
    where candidates = [(max 1 (n - 10 * (m - 1)))..(min 10 (n - m + 1))]

choose :: Integer -> Integer -> Integer
choose n k = product [(n-k+1)..n] `quot` product [1..k]

reorderings :: [Integer] -> Integer
reorderings xs = quot (product [1..20]) (product $ map (\n -> product [1..n]) xs)

colourProbability :: Integer -> Double
colourProbability n = product $ map (\i -> (fromIntegral i) / (fromIntegral 70)) [(10 - n + 1)..10]

sequenceProbability :: [Integer] -> Double
sequenceProbability xs = (product $ map colourProbability xs) * (fromIntegral $ reorderings xs)

probabilityDistinctColours :: Integer -> Double
probabilityDistinctColours n = (fromIntegral $ choose 7 n) * (sum . map sequenceProbability $ split n 20)

expectedDistinctColours :: Double
expectedDistinctColours = sum $ map (\n -> (fromIntegral n) * probabilityDistinctColours n) [0..7]

main = do
    print $ expectedDistinctColours
    print $ probabilityDistinctColours 0
    print $ probabilityDistinctColours 1
    print $ probabilityDistinctColours 2
    print $ probabilityDistinctColours 3
    print $ probabilityDistinctColours 4
    print $ probabilityDistinctColours 5
    print $ probabilityDistinctColours 6
    print $ probabilityDistinctColours 7
    print $ reorderings [10, 10]
    print . length $ split 7 20
