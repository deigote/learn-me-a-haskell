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


printListWithTitle :: (Show a) => String -> [a] -> IO [()]
printListWithTitle title list =
    let formattedTitle = " - " ++ title ++ ":"
    in  sequence $ (putStrLn formattedTitle):(map print list) ++ [putStrLn ""]

main = do
    printListWithTitle "applyTwice" tryApplyTwice
    printListWithTitle "zipWithMe" tryZipWithMe
