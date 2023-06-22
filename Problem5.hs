{-# LANGUAGE RankNTypes #-}
module Problem5 where

{-------------------------------------------------------------------------------

CS:3820 Fall 2021 Problem of the Week, Week 5
=============================================

This week's problem concerns data types and their folds.  There's a lot of text
this week, but not nearly as much code.

-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------

Problem 5-1
-----------

We'll start with a slight variation on the "append lists" that we developed in
Exercise 3.

-------------------------------------------------------------------------------}

data Appl a = One a | Many (Appl a) (Appl a)
  deriving (Eq, Show)

{-------------------------------------------------------------------------------

The idea is that a value of type `Appl a` represents a list of values of type
`a`.  The `One` case contains a single `a` value; the `Many` case contains two
sublists of `a`s.  Unlike the version developed in class, this version can only
represent *non-empty* lists.

Your task is to write a fold function `folda` for the type `Appl a`, with the
type:

    folda :: (a -> b) -> (b -> b -> b) -> Appl a -> b

An invocation of your fold function will look something like:

    folda f g as

where `f` is a function that turns single `a` values into `b` values and `g` is
a function that combines two `b` values to get a new `b` value.  Your function
should use `f` and `g` to turn `as`, of type `Appl a`, into a single value of
type `b`.

-------------------------------------------------------------------------------}

folda :: (a -> b) -> (b -> b -> b) -> Appl a -> b
folda x y (One a) = x a --represents the one a value
folda x y (Many a b) = y(folda x y a)(folda x y b)  --Produces the folds

-- >>> folda (\x -> x) (\x y -> x * y) (Many (Many (One 2) (One 3)) (One 4))
-- 24

{-------------------------------------------------------------------------------

Problem 5-2
-----------

Your next task is to write several functions that use your `folda` function.
But wait, you may be saying: how can we possibly *test* whether you've written
your functions using `folda` instead of using pattern matching directly?  Are we
breaking the rules already?

In fact, we're not.  Rather than write your functions directly in terms of your
`folda` function, each of your functions will take *a* folding function as its
first argument, and should be defined in terms of that folding function.  So
what does a folding function look like?  Something like this:

-------------------------------------------------------------------------------}

type FoldA = forall a b. (a -> b) -> (b -> b -> b) -> Appl a -> b

{-------------------------------------------------------------------------------

The `forall a b.` bit there is making explicit that the type variables `a` and
`b` should range over all types; don't worry about why we need to be explicit
about them here.

[I mean: do worry, ask me in office hours.]

Now we can specify each of the functions you need to write in terms of how
they'll behave given your `folda` function above... but we can test them using
different "folding" functions.

Your task is to write the following three functions:

    suma :: FoldA -> Appl Int -> Int

The invocation `suma folda as` should compute the sum of the values in `as`.

    longesta :: FoldA -> Appl [a] -> Int

In the invocation `longesta folda ls`, `ls` is an `Appl` of lists---that is to
say, it's a collection of lists.  `longesta folda ls` should evaluate the *the
length of* the longest list in `ls`.

    lista :: FoldA -> Appl a -> [a]

The invocation `lista folda as` should return a list containing the elements of
`as`, in the same (left-to-right) order in which they appear in `as`.

-------------------------------------------------------------------------------}

suma :: FoldA -> Appl Int -> Int
suma folda = folda x1 x2 --folds variable x1 and x2
                where x1 = \a -> a --x1 lambda a 
                      x2 = \a b -> a + b --x2 lambda a + b

-- >>> suma folda (Many (One 1) (Many (Many (One 2) (One 3)) (One 4)))
-- 10

longesta :: FoldA -> Appl [a] -> Int
longesta folda = folda length helperLong --branch to helper to "compare" ints

helperLong :: Int -> Int -> Int --Helper to compare ints
helperLong a b | a > b = a --if a > b then use a
               | otherwise = b --if not use b

-- >>> longesta folda (Many (Many (One "first") (One "second")) (Many (One "third") (One "fourth")))
-- 6

lista :: FoldA -> Appl a -> [a]
lista folda = folda listaX listaY --Takes helper lista lists and folds them

listaX :: a -> [a] --String to list of strings
listaX a = [a]

listaY :: [a] -> [a] -> [a] --list + list = new list
listaY a b = a ++ b

-- >>> lista folda (Many (Many (One "Hope") (Many (One "is") (One "the"))) (Many (Many (One "thing") (One "with")) (One "feathers")))
-- ["Hope","is","the","thing","with","feathers"]

{-------------------------------------------------------------------------------

Problem 5-3
-----------

Your next task is to write a mapping function, using your `folda` function.
That is, the invocation `mapa folda f as` should return a new `Appl` containing
the result of applying `f` to each value in `as`.

-------------------------------------------------------------------------------}

mapa :: FoldA -> (a -> b) -> Appl a -> Appl b --creates a new appended list with the results of applying f to the values
mapa folda f = folda (One . f) Many --folds the appended lists and applies f to each value
               where convAtoB = \(One a) f -> One (f a) -- converts a to a given "b"
                     addLists = \a b -> Many a b --Lambda producing many // adds lists together

-- >>> mapa folda (1+) (Many (One 1) (Many (One 2) (Many (One 3) (One 4))))
-- Many (One 2) (Many (One 3) (Many (One 4) (One 5)))

{-------------------------------------------------------------------------------

Problem 5-4
-----------

Earlier, I said that we'd test your functions by providing folding functions
other than your `folda` function.  Your final task is to define one such
alternative folding function.  The difference between this function, `folda'`,
and your original `folda` function should be that this one *reverses* the
branches of the tree as it works through them.  Concretely, while the result of

    mapa folda id as

should be identical to `as`, the result of

    mapa folda' id as

should be a mirror image of the input `as`.

-------------------------------------------------------------------------------}

folda' :: FoldA --folda' is folda but with flipped variables
folda' x y (One a) = x a 
folda' x y (Many a b) = y (folda' x y b) (folda' x y a) --Produces the reversed folds from folda

-- >>> mapa folda' id (Many (Many (One 1) (One 2)) (One 3))
-- Many (One 3) (Many (One 2) (One 1))

-- >>> lista folda' (Many (Many (One "Hope") (Many (One "is") (One "the"))) (Many (Many (One "thing") (One "with")) (One "feathers")))
-- ["feathers","with","thing","the","is","Hope"]
