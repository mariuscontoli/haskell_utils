mySucc :: Int -> Int
mySucc x = x + 1

myIsNeg :: Int -> Bool
myIsNeg x = x < 0

myAbs :: Int -> Int 
myAbs x
    | x < 0 = -x
    | otherwise = x

myMin :: Int -> Int -> Int
myMin x y 
    | x < y = x
    | otherwise = y

myMax :: Int -> Int -> Int
myMax x y 
    | x > y = x
    | otherwise = y

myTuple :: a -> (b -> (a, b))
myTuple a b = (a, b)

myTruple :: a -> b -> c -> (a, b, c)
myTruple a b c = (a, b, c)

myFst :: (a, b) -> a
myFst (a, b) = a

mySnd :: (a, b) -> b
mySnd (a, b) = b

mySwap :: (a, b) -> (b, a)
mySwap (a, b) = (b, a)

myHead :: [a] -> a
myHead [] = error "Empty list !"
myHead (a:_) = a

myTail :: [a] -> [a]
myTail [] = error "Empty list !"
myTail (a:b) = b

myLength :: [a] -> Int 
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myNth :: [a] -> Int -> a
myNth [] index = error "Empty list !"
myNth (x:xs) 0 = x
myNth (x:xs) index 
    | index < 0 = error "Negative index"
    | index > myLength (x:xs) = error "Outbounding index" 
    | otherwise = myNth xs $ index - 1

myTake :: Int -> [a] -> [a]
myTake index [] = []
myTake 0 _ = []
myTake index (x:xs)
    | index < 0 = error "Negative index"
    | otherwise = x:myTake (index - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop index xs@(_:xs')
    | index > 0 = myDrop (index - 1) xs'
    | otherwise = xs

myAppend :: [a] -> [a] -> [a]
myAppend [] [] = []
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

myRev :: [a] -> [a] -> [a]
myRev [] acc = acc
myRev (x:xs) acc = myRev xs (x:acc)  

myReverse ::  [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myRev xs [x]

myInit :: [a] -> [a]
myInit [] = error "Empty list !"
myInit ls = myTake (myLength ls - 1) ls

myLast :: [a] -> a
myLast [] = error "Empty List !"
myLast ls = myNth ls (myLength ls - 1)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myUnzip :: [(a, b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x,y):xs) = (x:list1, y:list2)
    where (list1, list2) = myUnzip xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap func (x:xs) = func x : myMap func xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter func (x:xs)
    | func x = x:myFilter func xs
    | otherwise = myFilter func xs

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _  b [] = b
myFoldl func b (x:xs) = myFoldl func (func b x) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr func b (x:xs) = func x (myFoldr func b xs)

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition func ls = (myFilter func ls, myFilter (not . func) ls)

myQuickSort :: (a -> a -> Bool) -> [a] -> [a]
myQuickSort func [] = []
myQuickSort func (x:xs)
    = myAppend (myQuickSort func list1) (x:myQuickSort func list2)
        where (list1, list2) = myPartition (not . func x) xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem element (x:xs)
    | element == x = True
    | otherwise = myElem element xs

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv op op2 = Just (div op op2)

safeNth :: [a] -> Int -> Maybe a
safeNth [] index = Nothing
safeNth (x:xs) 0 = Just x
safeNth (x:xs) index
    | index < 0 = Nothing
    | index > myLength (x:xs) = Nothing
    | otherwise = safeNth xs $ index - 1

safeSucc :: Maybe Int -> Maybe Int
safeSucc Nothing = Nothing
safeSucc x = fmap succ x

myLookup :: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup element ((x,y):xs)
    | element == x = Just y
    | otherwise = myLookup element xs

maybeDo :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeDo _ Nothing _ = Nothing 
maybeDo _ _ Nothing = Nothing
maybeDo func ma mb = ma >>=  (\a -> mb >>= (\b -> Just (func a b)))

readInt :: [Char] -> Maybe Int
readInt str = case reads str of
        [(x, "")] -> Just x
        _ -> Nothing

getLineLength :: IO Int
getLineLength = length <$> getLine

printAndGetLength :: String -> IO Int
printAndGetLength str = putStrLn str >> return (length str)

getInt :: IO (Maybe Int)
getInt = readInt <$> getLine

getLines :: Int -> String -> IO String
getLines 0 line = return line
getLines n line = do
    lines <- getLine
    getLines (n - 1) (line ++ lines)

concatLines :: Int -> IO String
concatLines 0 = return ""
concatLines n = getLines n ""

main :: IO ()
main = return ()

printBox :: Int -> IO ()
printBox 0 = error "Number is zero."
printBox n 
    | n < 0 = error "Number is negative"
    | otherwise = printLine n 0

printLine :: Int -> Int -> IO ()
printLine 1 _ = putStrLn "++"
printLine max 0 =
            putStrLn ("+" ++ replicate(max * 2 - 2)'-' ++ "+") >>
            printLine max 1
printLine max i
        | i == (max - 1) = 
            putStrLn ("+" ++ replicate(max * 2 - 2)'-' ++ "+")
        | otherwise =
            putStrLn ("|" ++ replicate(max * 2 - 2)' ' ++ "|") >>
            printLine max (i + 1)