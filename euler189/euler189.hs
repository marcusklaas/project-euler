import Data.List

data Colour = Red | Green | Blue deriving (Show, Eq)

otherColours :: Colour -> [Colour]
otherColours c = distinctColours c c

distinctColours :: Colour -> Colour -> [Colour]
distinctColours a b = delete a $ delete b [Red, Green, Blue]
    
takeEvenIndex :: [a] -> [a]
takeEvenIndex (x:y:xs) = x:takeEvenIndex xs
takeEvenIndex xs       = xs

-- take a stub and return all ways to complete it (by inspersing colours that aren't the same)
finishHelper :: [Colour] -> [[Colour]]
finishHelper (x:y:xs) = [x] : distinctColours x y : finishHelper (y:xs)
finishHelper [x]      = [x] : [otherColours x]

-- TODO: use @as
finishRow :: [Colour] -> [[Colour]]
finishRow (x:xs) = sequence $ otherColours x : finishHelper (x:xs)

-- take even indexed elements and take all combinations of the mapping by otherColours
getStubs :: [Colour] -> [[Colour]]
getStubs row = sequence $ fmap otherColours $ takeEvenIndex row

viableContinuations :: [Colour] -> [[Colour]]
viableContinuations row = concatMap finishRow $ getStubs row

rowContinuations :: Int -> [Colour] -> Integer
rowContinuations iters row
    | iters == 1 = 1
    | otherwise  = foldl' (+) 0 $ map (rowContinuations (iters - 1)) $ viableContinuations row

main = print $ 3 * (rowContinuations 5 [Red])
