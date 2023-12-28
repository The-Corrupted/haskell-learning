myDrop n xs | n <= 0 = xs
myDrop _ [] = []
myDrop n (_:xs) = myDrop (n-1) xs

isEven :: Int -> Bool
isEven n = mod n 2 == 0

printBool :: Bool -> String
printBool b = if b
    then "True"
    else "False"

data Book = Book String Int
  deriving(Show)

bookTitle :: Book -> String
bookYear :: Book -> Int

bookTitle (Book title _) = title
bookYear (Book _ year) = year

sumList :: [Int] -> Int
sumList (x:xs) = x + sumList xs
sumList _ = 0

data List a = Cons a (List a)
  | Nil
  deriving(Show)

listVal :: List a -> a
listVal (Cons x _) = x
nextList :: List a -> List a
nextList (Cons _ next) = next


data Tree a = Node a (Tree a) (Tree a)
  | Empty
  deriving(Show)

toList :: [Int] -> List Int
toList (x:xs) = Cons x (toList xs)
toList _ = Nil

fromList :: List a -> [a]
fromList x = case x of
  (Cons y _) -> [y] ++ fromList (nextList x)
  _ -> []

len :: [a] -> Int
len [] = 0
len (_: xs) = 1 + (len xs)

mean :: [Int] -> Int
mean (x:xs) | len xs <= 1 = x
mean [] = 0
mean xs = (sumList xs) `div` (len xs)

palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ (myDrop 1 (reverse xs))

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs | (len xs) == 1 = True
isPalindrome (x:xs) = if x == (last xs)
    then isPalindrome (init xs)
	 else False


main :: IO ()
main = do
  let list = toList [1,2,3,4,5,6]
  let arr = fromList list
  let book = Book "Some Book" 1999
  let title = bookTitle book
  let num = 99
  let tailEq = myDrop 2 "Hello" == "llo"
  putStrLn (show (len "Hello World") :: String)
  putStrLn (show list)
  putStrLn (show arr)
  putStrLn (show (sumList [1,2,3,4,5,6,7,8,9]))
  putStrLn (title)
  putStrLn ("TailEq llo " ++ printBool tailEq)
  putStrLn ((show num :: String) ++ " " ++ printBool (isEven 8))
  putStrLn (myDrop 3 "Hello World")
  putStrLn (reverse "Hello")
