naturalNumbers = [1..999]

divisibleByThreeOrFive = filter div3or5 naturalNumbers
    where div3or5 x = x `mod` 5 == 0 || x `mod` 3 == 0

main = print (sum divisibleByThreeOrFive)
