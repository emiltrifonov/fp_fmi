myAbs x = if x < 0 then -x else x

isTriangle a b c
    | a <= 0 || b <= 0 || c <= 0 = error "Sides must be positive"
    | a + b > c && a + c > b && b + c > a = True
    | otherwise = False

isLeapYear :: Integer -> Bool
isLeapYear year
    | (year `mod` 4 == 0 && year `mod` 100 /= 0) || (year `mod` 400 == 0) = True
    | otherwise = False

daysInMonth :: Integer -> Integer -> Integer
daysInMonth 1 _ = 31
daysInMonth 2 year = if isLeapYear year then 29 else 28
daysInMonth 3 _ = 31
daysInMonth 4 _ = 30
daysInMonth 5 _ = 31
daysInMonth 6 _ = 30
daysInMonth 7 _ = 31
daysInMonth 8 _ = 31
daysInMonth 9 _ = 30
daysInMonth 10 _ = 31
daysInMonth 11 _ = 30
daysInMonth 12 _ = 31

countDays :: Integer -> Integer -> Integer -> Integer
countDays d m y = sum (map (\month -> daysInMonth month y) [1..(m - 1)]) + d

checkDivisors :: Integer -> Integer -> Bool
checkDivisors num div
    | div * div > num = True
    | num `mod` div == 0 = False
    | otherwise = checkDivisors num (div + 2)

isPrime :: Integer -> Bool
isPrime x
    | x <= 1 = False
    | x == 2 = True
    | even x = False
    | otherwise = checkDivisors x 3

getSum :: Int -> Int -> Int
getSum num div
    | num == div = div
    | num `mod` div == 0 = div + getSum num (div + 1)
    | otherwise = getSum num (div + 1)

sumDivisors :: Int -> Int
sumDivisors x
    | x == 0 = 0
    | x < 0 = sumDivisors (myAbs x)
    | otherwise = getSum x 1

isPerfectNumber :: Int -> Bool
isPerfectNumber x = (sumDivisors x) == x * 2

countBinaryWithout0 :: Int -> Int
countBinaryWithout0 x
    | x == 0 = 0
    | otherwise = 1 + countBinaryWithout0(x `div` 2)
countBinaryDigits :: Int -> Int
countBinaryDigits x
    | x == 0 = 1
    | otherwise = countBinaryWithout0 x

countOnes :: Int -> Int
countOnes x
    | x == 0 = 0
    | x < 0 = countOnes(myAbs x)
    | even x = countOnes(x `div` 2)
    | otherwise = 1 + countOnes(x `div` 2)
isEvil :: Int -> Bool
isEvil x
    | x < 0 = isEvil (myAbs x)
    | otherwise = even (countOnes x)

sumEvil :: Int -> Int -> Int
sumEvil a b
    | a > b = sumEvil b a
    | a == b = if isEvil a then a else 0
    | otherwise = if isEvil a then a + sumEvil (a + 1) b else sumEvil (a + 1) b

compose f g x = f (g x)
