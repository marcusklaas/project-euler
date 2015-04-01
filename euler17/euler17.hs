-- Made in collaboration with https://github.com/JordyMoos

belowTenList = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

belowTwentyList = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

multiplesOfTen = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

totalLength :: [String] -> Int
totalLength = sum . (map length)

belowHundredLength = totalLength belowTwentyList
                     + totalLength multiplesOfTen * 10
                     + totalLength belowTenList * (length multiplesOfTen + 1)

upToThousandLength = belowHundredLength * 10
                     + length "hundred" * 900
                     + totalLength belowTenList * 100
                     + length "and" * 891
                     + length "onethousand"

main = print upToThousandLength
