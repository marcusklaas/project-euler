import MaximumPath
import System.IO
import System.Environment
import Data.List.Split

readIntList :: String -> [Int]
readIntList str = map read $ splitOn " " str

process :: String -> Int
process str = getMaxPath [] $ reverse $ map readIntList $ lines str 

main :: IO ()
main = do 
    args <- getArgs
    if length args == 1
        then do
            input <- openFile (head args) ReadMode
            inputString <- hGetContents input
            print $ process inputString
        else do
            putStrLn "Input file missing!"
