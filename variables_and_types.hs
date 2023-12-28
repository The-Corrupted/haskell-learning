-- We can create our own type and constructor to create a new
-- instance by using the data keyword. 
-- This creates a new type called Book that takes and holds a string
-- and integer
data Book = Book String Integer
    deriving(Show)
-- We can add additional functionality to our type by deriving
-- Right now we don't have a good way of accessing the data inside
-- book so we need to define an accessor function
bookTitle :: Book -> String
bookTitle (Book title _) = title
-- Now we can access the book title by calling the bookTitle function
-- on an instance of Book. We need to do this for the Integer as well.
bookYear :: Book -> Integer
bookYear (Book _ year) = year
-- This is extremely tedious to do so a second way of defining a type
-- is provided. Records. This allows us to name the types and the
-- accessor functions are automatically generated based on the name.
data Calendar = Calendar {
    day :: Integer,
    month :: Integer,
    year :: Integer
} deriving(Show)

-- Now we have a type that can be contructed with placeholders for values
-- and gives us the ability to access the various items inside it using
-- the automatically generated accessor functions

-- We can also create a type alias using the type keyword
type Name = String
type Age = Integer
type Height = Double
data Person = Person {
    name :: Name,
    age :: Age,
    height :: Height
} deriving(Show)

main :: IO ()
main = do
    -- Defining a variable with its type ( Integer )
    -- :: is what's used to specify the type of a variable.
    -- if this is left out then the compiler will try to
    -- infer the type of the variable
    let some = 0 :: Integer
    -- Defining double. It's recommended to use double over float
    -- since it's faster in most cases
    let some2 = 0.0 :: Double
    -- Let the compiler infer the type ( String )
    let string = "Some String"
    -- String type
    let string2 = "Some String" :: [Char]
    -- Strings in haskell are techinally char lists
    -- but you can also define them as String which
    -- is just an alias of [Char]
    let string2 = "Some String" :: String
    -- Lists are defined as a type inside brackets, much
    -- like a string.
    let intList = [1, 2, 3, 4, 5, 6] :: [Integer]
    -- This creates a new type called book with the constructor Book
    -- and it holds two values. A string and an integer
    -- to construct a new book we just call the constructor
    let book = Book "Some Book" 1999
    -- Creating an instance of calendar using the more verbose syntax
    let calendar = Calendar {
        day = 12,
        month = 9,
        year = 1995}
    -- Accessing the book title using our constructor
    let title = bookTitle book
    -- Accessing the year using the automatically generated constructor
    let cYear = year calendar
    -- We also have tuples which are fixed length and can combine types
    let tup = (99, "SomeString", 9.0) :: (Integer, String, Double)
    -- You cannot reassign to a variable but if you use let with the same
    -- name of an existing variable then you will shadow it and all
    -- further references to that variable will use the value that's
    -- been assigned ( and the other value is [kind of] lost )
    let tup = (0.0, 9, [1, 2, 3]) :: (Double, Integer, [Integer])
    print (show cYear :: String)

