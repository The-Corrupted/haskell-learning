-- Functions are defined similarly to new types.
-- You give the function a name and optionally a type signature.
mySum :: (Integral a) => [a] -> a
-- The above defined a new function that takes a list of type a
-- and returns type a. Type a is best understood as a generic.
-- (Integral a) is a type constraint which tells the compiler
-- that a can be any integer type. (So passing [Char] or String
-- to the function will trigger an error)

-- The definition of a function is the function name, the value it takes
-- an equal sign and then an expression
mySum (x:xs) = if null xs
        then x
    else x + mySum xs
-- The above code defines the sum function and specifies it's behavior.
-- Special syntax was used in the above (x:xs) is more or less shorthand
-- for tail xs which removes the first item of the list and returns the
-- remainder of the list. In the above case x is the first item and xs
-- is the remainder. In the above function we specify that if the list
-- isn't empty or null then we add the first element to the value of 
-- sum xs. If it is nil we return x

-- The above has a bug though. If we pass an empty list to it then we
-- get an error that there was a Non-exhaustive pattern in the function.
-- What this means is that we don't account for instances where the
-- list is empty and the above "pattern" or function definition expects
-- that at least one item exists in the list. We can fix this a number
-- of different ways. The first is with guards.
mySum xs | null xs = 0
-- The above code creates a "pattern" or guard for this function that
-- tells it that if our list, xs is null then we should return zero.
mySum [] = 0
-- This can be done a bit more succiently with the above code which is
-- a pattern match that does exactly the same thing as the guard
-- but with less code. Guards can be chained though so if you want
-- to match several patterns at once then the guard might be the 
-- better option

-- Lets do something more difficult and create a linked list type and
-- implement some functions for working with it.
data LinkedList a = Cons a (LinkedList a)
    | Empty
    deriving(Show)
nextList :: LinkedList a -> LinkedList a
nextList (Cons _ next) = next

arrToList :: [a] -> LinkedList a
arrToList (x:xs) = Cons x (arrToList xs)
arrToList [] = Empty

listToArr :: LinkedList a -> [a]
listToArr x = case x of
    (Cons y _) -> [y] ++ listToArr (nextList x)
    _ -> []

-- The above creates a Linked list type and gives us some ways of turning
-- it into a haskell list and taking a haskell list and turn it into
-- our linked list type.

-- nextList is a function that uses pattern matching to get the next node
-- in the linked list ( if the list isn't empty then we get the next
-- value in the list
--
-- similarly listToArr uses the haskell case statement which allows us to
-- match the value of a variable and perform an action ( or extract 
-- some value ) if it matches. In the above case we match if the list
-- isn't empty and assign the internal value to a list. We then
-- append the result of listToArr (nextList x) and return. If we've
-- reached the end of the list then we just return an empty list.
--
-- The _ in the case expression and nextList function is a wildcard that
-- means match anything and discard. You use this in Rust for match 
-- statements but in other languages this might show up as default. By
-- using _ we're telling the compiler that any value other than the
-- ones matched above should trigger an empty list return
