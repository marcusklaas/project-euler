import Text.Printf ( printf )

split :: Integer -> Integer -> [[Integer]]
split 1 n = [[n]]
split m n = [x : rest | x <- candidates, rest <- split (m - 1) (n - x)]
    where candidates = [(max 1 (n - 10 * (m - 1)))..(min 10 (n - m + 1))]

choose :: Integer -> Integer -> Integer
choose n k = product [(n-k+1)..n] `quot` product [1..k]

reorderings :: [Integer] -> Integer
reorderings xs = quot (product [1..20]) (product $ map (\n -> product [1..n]) xs)

colourProbability :: Integer -> Double
colourProbability n = fromIntegral $ product [(10 - n + 1)..10]

sequenceProbability :: [Integer] -> Double
sequenceProbability xs = (fromIntegral $ reorderings xs) * (product $ map colourProbability xs) / (fromIntegral $ product [51..70])

probabilityDistinctColours :: Integer -> Double
probabilityDistinctColours n = (fromIntegral $ choose 7 n) * (sum . map sequenceProbability $ split n 20)

expectedDistinctColours :: Double
expectedDistinctColours = sum $ map (\n -> (fromIntegral n) * probabilityDistinctColours n) [1..7]

main = printf "%.9f\n" expectedDistinctColours
