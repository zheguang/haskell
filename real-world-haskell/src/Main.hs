module Main where

main :: IO ()
main = do
  putStrLn "hello world"


data List a = Nil
            | Cons a (List a)
            deriving (Show)

-- Converse of 'fromList' that takes a 'List a' and generates [a]
converseFromList :: List a -> [a]
converseFromList Nil = []
converseFromList (Cons x xs) = [x] ++ converseFromList xs
-- parenthesis around pattern matching, argument comprehension is important.

-- A data type Tree with a sole constructor
data Tree2 a = Node2 a (Maybe [Tree2 a]) deriving (Show)


-- Computes the length of a 'List'
lengthOfList :: List a -> Int
lengthOfList Nil = 0
lengthOfList (Cons x xs) = 1 + lengthOfList xs


-- Computes the mean of a 'List'
meanOfList :: List Int -> Maybe Float
meanOfList Nil = Nothing
meanOfList xs =  Just ((fromIntegral $ sumOfList xs) / (fromIntegral $ lengthOfList xs))
  where sumOfList :: List Int -> Int
        sumOfList (Cons x xs) = x + sumOfList xs
        sumOfList Nil = 0


-- Turns a 'List' into a palindrome
palindromeFromList :: [a] -> [a]
palindromeFromList [] = []
palindromeFromList xs = xs ++ reverseOfList xs
  where reverseOfList :: [a] -> [a]
        reverseOfList [] = []
        reverseOfList (x:xs) = reverseOfList xs ++ [x]

-- Determines whether a list is a palindrome
isPalindrome :: [Int] -> Bool
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome xs = (firstOfList xs == lastOfList xs) && (isPalindrome $ middleOfList xs)
  where firstOfList (x:_) = x
        lastOfList (x:[]) = x
        lastOfList (_:xs) = lastOfList xs
        middleOfList (_:xs) = prefixOfList xs
          where prefixOfList (x:[]) = []
                prefixOfList (x:xs) = x:prefixOfList xs


sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength [] = []
sortBySublistLength (x:xs) = (sortBySublistLength $ lessThan x xs) ++ [x] ++ (sortBySublistLength $ atLeast x xs)
  where lessThan x ys = filter (\y -> length y < length x) ys
        atLeast x ys = filter (\y -> length y >= length x) ys


-- join list of lists together using a separator value
intersperse :: a -> [[a]] -> [a]
intersperse x [] = []
intersperse x (ys:[]) = ys
intersperse x (ys:yss) = ys ++ [x] ++ intersperse x yss


data Tree a = Empty
            | Node a (Tree a) (Tree a)
            deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ leftChild rightChild) = 1 + max (treeHeight leftChild) (treeHeight rightChild)


data Direction = DirLeft | DirRight | DirStraight deriving (Show)

directionFrom :: Float -> Direction
directionFrom sineTheta
  | sineTheta > 0 = DirLeft
  | sineTheta < 0 = DirRight
  | otherwise = DirStraight

data Point2D = Point2D { x :: Float, y :: Float } deriving (Show)

directionOfTurn :: Point2D -> Point2D -> Point2D -> Direction
directionOfTurn a b c = directionFrom $ sineOf (vectorOf a b) (vectorOf b c)
  where vectorOf p1 p2 = Point2D { x = (x p2) - (x p1), y = (y p2) - (y p1) }
        sineOf :: Point2D -> Point2D -> Float
        sineOf v1 v2 = (cross v1 v2) / (norm v1 * norm v2)
          where cross :: Point2D -> Point2D -> Float
                cross v1 v2 = x v1 * y v2 - y v1 * x v2
                norm :: Point2D -> Float
                norm v = sqrt $ (x v)^2 + (y v)^2


-- chapter 4
-- safe partial list functions
