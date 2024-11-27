import Prelude hiding (length, foldr, foldl, reverse, init, product, zip, zipWith, forall, last)

-- 1
length :: [a] -> Int 
length = foldr (\_ acc -> acc + 1) 0

-- 2
exists :: (a -> Bool) -> [a] -> Bool
exists p = foldr ((||) . p) False

-- 3
myforall :: (a -> Bool) -> [a] -> Bool
myforall p = foldr ((&&) . p) True

-- 4
member :: Eq a => a -> [a] -> [a]
member n [] = []
member n (x : xs) = if x == n then x:xs else member n xs

-- 5
push :: a -> [a] -> [a]
push x xs = xs ++ [x]

-- 6
reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = reverse xs ++ [x]

-- 7
init :: [a] -> [a]
init [] = error "Empty list"
init [x] = []
init (x : xs) = x : init xs

-- 8
insert :: a -> Int -> [a] -> [a]
insert x n [] = [x]
insert x 1 xs = x : xs
insert x n (xx : xs) = xx : insert x (n - 1) xs

-- 9
--foldr op init l
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op init [] = init
foldr op init (x : xs) = op x (foldr op init xs)

-- 10
--foldl op init l
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op init [] = init
foldl op init (x : xs) = foldl op (op init x) xs

-- 11
product :: [Int] -> Int
product = foldr (*) 1

-- 12
zip :: [a] -> [b] -> [(a,b)]
zip [] [] = []
zip as [] = []
zip [] bs = []
zip (a:as) (b:bs) = (a, b) : zip as bs

-- 13
zipwith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipwith _ [] [] = []
zipwith _ _ [] = []
zipwith _ [] _ = []
zipwith f (x : xs) (y : ys) = f x y : zipwith f xs ys

-- 14
interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave _ [] = []
interleave [] _ = []
interleave (x : xs) (y : ys) = x : y : interleave xs ys

-- 15
nats :: [Int]
nats = [0..]

-- 16
pythagoreanTriples :: [(Int, Int, Int)]
pythagoreanTriples = [ (x, y, z) | x <- [1..30], y <- [1..30], z <- [1..30], x * x + y * y == z * z && x < y && y < z]

-- 17
fibb :: [Int]
fibb = 0 : 1 : zipwith (+) fibb (tail fibb)

-- 18
primes :: [Int]
primes = sieve [2..]
    where
        sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

main :: IO()
main = do
    let l = [1,2,3,5,10]
    let n = [-2, 6, 20, 14]
    let l2 = ["word", "card", "paper"]
    print (take 10 primes)