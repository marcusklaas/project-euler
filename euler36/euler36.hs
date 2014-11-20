toBinary :: Int -> String
toBinary 1 = "1"
toBinary 0 = "0"
toBinary x = toBinary (x `quot` 2) ++ (toBinary $ x `mod` 2)

isDoubleBasePalindrome :: Int -> Bool
isDoubleBasePalindrome x = dec == reverse dec && bin == reverse bin
    where dec = show x
          bin = toBinary x
          
main = print $ sum $ filter isDoubleBasePalindrome [1..10^6]   
