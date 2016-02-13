

applyTwice :: (t -> t) -> t -> t
applyTwice func x = func (func x)

tryApplyTwice = [applyTwice (+2) 3, applyTwice (*5) 2]




printListWithTitle :: (Show a) => String -> [a] -> IO [()]
printListWithTitle title list =
    let formattedTitle = " - " ++ title ++ ":"
        formattedList = map show list
    in  sequence $ (putStrLn formattedTitle):(map print formattedList) ++ [putStrLn ""]

main = do
    printListWithTitle "applyTwice" tryApplyTwice
