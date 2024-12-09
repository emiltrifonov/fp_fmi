import Data.Char
import Data.List
import Data.Foldable

--zad 1
whisper :: String -> String
whisper = map toLower

--zad 2
removeSpaces :: String -> String
removeSpaces = filter (not . isSpace)

--zad 3
switchCaps :: String -> String
switchCaps = map (\x -> if isLower x then toUpper x else toLower x)

--zad 4
encrypt :: Int -> String -> String
encrypt n str = ""

--zad 5
joinWords :: Char -> [String] -> String
joinWords _ [] = []
joinWords c str = tail (concat [c : s | s <- str])

--zad 6

--zad 7
indices :: Int -> [Int] -> [Int]
indices _ [] = []
indices n nums = [ind | (x,ind) <- zip nums [0..], x == n]

--zad 8
lastIndex :: Int -> [Int] -> Int
lastIndex n xs = last [ind | (x, ind) <- zip xs [0..], x == n]

--zad 9
countMin :: Ord a => [a] -> Int
countMin xs = length [x | x <- xs, x == m] where m = minimum xs

--zad 10
primeReorder :: [a] -> [a]
primeReorder xs = []

--zad 11
dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (x:xs) = x : dedup [y | y <- xs, y /= x]

--zad 13
subsets [] = [[]]
subsets (x:xs) = map (x:) subs ++ subs
    where subs = subsets xs

--zad 14
pick :: (Eq t, Num t) => t -> [a] -> [[a]]
pick 0 _ = [[]]
pick _ [] = []
pick k (x:xs) = map (x:) (pick (k - 1) xs) ++ pick k xs

--zad 15
maximize fs x = maximum $ [f arg | (f,arg) <- zip fs rep] 
    where rep = [x | _ <- [1..length fs]]

--zad 16
compose :: [a -> a] -> a -> a
compose [f] x = f x
compose (f:fs) x = f (compose fs x)

--zad 17 -- 1 2 3 4 5 6 * 2 3 4 5 6 7
helper :: [Integer]
helper = [3..]
facts :: [Integer]
facts = 1 : 2 : zipWith (*) helper (tail facts)

--better with chatgpt
facts2 = tail (scanl (*) 1 [1..])

--18
points = [(x,y) | n <- [0..], x <- [0..n], let y = n - x]

main :: IO ()
main = do
    print $ take 10 points

