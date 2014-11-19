productList = [ x * y | x <- [100..999], y <- [100..999]]

isPalindromeNumber :: Int -> Bool
isPalindromeNumber n = show n == reverse (show n)

palindromes = filter isPalindromeNumber productList

answer = maximum palindromes

main = print (answer)
