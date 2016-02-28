applyTwice :: (t -> t) -> t -> t
applyTwice func x = func (func x)

tryApplyTwice = [
        show $ applyTwice (+2) 3,
        show $ applyTwice (*5) 2,
        applyTwice (++ " Haha") "Nelson says"
    ]

zipWithMe :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithMe _ [] _ = []
zipWithMe _ _ [] = []
zipWithMe zipper (h1:t1) (h2:t2) = zipper h1 h2 : zipWithMe zipper t1 t2

tryZipWithMe = [
        map show (zipWithMe (*) [1,2,3] [4,5,6]),
        map show (zipWithMe (+) [1,2,3] [4,5,6]),
        [concat $ zipWithMe (\x y -> x:[y]) "Imhpy" "' ap!"]
    ]

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f y x = f x y

tryMyFlip = [
        map show $ myFlip zip [1, 2, 3] "hello",
        myFlip (zipWithMe (\x y -> x:[y])) ['1', '2', '3'] "hello"
    ]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
    | p x = x:rest
    | otherwise = rest
    where rest = myFilter p xs

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' p (x:xs) = if p x then x:rest else rest
    where rest = myFilter p xs

tryMyFilter = [
        myFilter (>5) [1,2,5,9,10],
        myFilter (5>) [1,2,5,9,10],
        myFilter' (>5) [1,2,5,9,10],
        myFilter' (5>) [1,2,5,9,10]
    ]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let bigger = myFilter (>=x) xs
        smaller = myFilter (x>) xs
    in  (quickSort smaller) ++ x:(quickSort bigger)

tryQuickSort = [
        concat $ map show $ quickSort [5,3,1,2,8,4,3,1],
        quickSort "chachi que si"
    ]

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x = x:myTakeWhile p xs
    | otherwise = []

sumAllOddSquaresSmallerThan :: Int -> Int
sumAllOddSquaresSmallerThan x = sum $ takeWhile (<x) (filter odd (map (^2) [1..]))

sumAllOddSquaresSmallerThan' :: Int -> Int
sumAllOddSquaresSmallerThan' x = sum $ takeWhile (<x) [n | n <- map (^2) [1..x], odd n]

tryMyTakeWhile = [
        sumAllOddSquaresSmallerThan 10000,
        sumAllOddSquaresSmallerThan' 10000
    ]

collatzNext :: (Integral a) => a -> a
collatzNext x
    | odd x = x * 3 + 1
    | otherwise = x `div` 2

collatzSequence :: (Integral a) => a -> [a]
collatzSequence 1 = [1]
collatzSequence x = x:(collatzSequence $ collatzNext x)

collatzLenghtGreaterThan  :: (Integral a) => a -> Int -> Int
collatzLenghtGreaterThan maxSeed minLength = length $ filter gtMinLength (map collatzSequence [1..maxSeed])
    where gtMinLength x = length x > minLength

tryCollatzSequence = map collatzSequence [4,30,200]

tryCollatzLenghtGreaterThan = [
        collatzLenghtGreaterThan 100 15,
        collatzLenghtGreaterThan 1000 15,
        collatzLenghtGreaterThan 10000 15
    ]

howManyForTheSumOfRootsOfNumbersToExceed :: Float -> Int
howManyForTheSumOfRootsOfNumbersToExceed x = length $ takeWhile (<=x) (scanl (+) 0 (map sqrt [1..]))

tryHowManyForTheSumOfRootsOfNumbersToExceed = [
        howManyForTheSumOfRootsOfNumbersToExceed 10,
        howManyForTheSumOfRootsOfNumbersToExceed 1000
    ]

sumOfOddSquaresSmallerThan :: Int -> Int
sumOfOddSquaresSmallerThan x = sum $ takeWhile (<x) (filter odd (map (^2) [0..]))

sumOfOddSquaresSmallerThan' :: Int -> Int
sumOfOddSquaresSmallerThan' x = sum $ takeWhile (<x) . filter odd . map (^2) $ [0..]

trySumOfOddSquaresSmallerThan = [
        sumOfOddSquaresSmallerThan 10000,
        sumOfOddSquaresSmallerThan' 10000
    ]

printListWithTitle :: (Show a) => String -> [a] -> IO [()]
printListWithTitle title list =
    let formattedTitle = " - " ++ title ++ ":"
    in  sequence $ (putStrLn formattedTitle):(map print list) ++ [putStrLn ""]

main = do
    printListWithTitle "applyTwice" tryApplyTwice
    printListWithTitle "zipWithMe" tryZipWithMe
    printListWithTitle "myFlip" tryMyFlip
    printListWithTitle "myFilter" tryMyFilter
    printListWithTitle "quickSort" tryQuickSort
    printListWithTitle "myTakeWhile" tryMyTakeWhile
    printListWithTitle "collatzSequence" tryCollatzSequence
    printListWithTitle "collatzLenghtGreaterThan" tryCollatzLenghtGreaterThan
    printListWithTitle "howManyForTheSumOfRootsOfNumbersToExceed" tryHowManyForTheSumOfRootsOfNumbersToExceed
    printListWithTitle "sumOfOddSquaresSmallerThan" trySumOfOddSquaresSmallerThan