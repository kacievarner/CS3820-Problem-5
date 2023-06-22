module Tests where

-- GHC
import System.Exit
import System.Environment

-- External
import Test.HUnit
import Data.Char (toUpper)
-- import Test.QuickCheck

-- Lib
import Problem5

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

next :: Int -> Int
next x | even x    = x `div` 2
       | otherwise = 3 * x + 1


binTree :: Int -> e -> Appl e
binTree 0 e = One e
binTree n e = Many branch branch where
  branch = (binTree (n - 1) e)

t, t' :: Appl Int
t = Many
  (Many (One 1) (One 2))
  (Many (One 3) (One 4))

t' = Many
  (Many (One 4) (One 3))
  (Many (One 2) (One 1))

foldFst, foldLst :: FoldA

foldFst f _ (One a) = f a
foldFst f g (Many l r) = foldFst f g l

foldLst f _ (One a) = f a
foldLst f g (Many l r) = foldLst f g r

p51 :: Test
p51 = test $
  [ folda id (*) (binTree 2 n) @?= n^4 | n <- [1..10]] ++
  [
    folda (map toUpper) (++) (binTree 2 "a") @?= "AAAA",
    folda show (++) t @?= "1234"
  ]
  
p52 = test $
  [
    suma folda t                               @?= 10,
    suma folda (binTree 10 1)                  @?= 2^10,
    longesta folda (One "a")                   @?= 1,
    longesta folda (Many (One "a") (One "bc")) @?= 2,
    longesta folda (binTree 10 "asdf")         @?= 4,
    lista folda t                              @?= [1, 2, 3, 4],

    -- alternative folds
    suma foldFst t @?= 1,
    suma foldLst t @?= 4,
    longesta foldFst (Many (One "a") (One "bc")) @?= 1,
    longesta foldLst (Many (One "a") (One "bc")) @?= 2,
    lista foldFst t @?= [1],
    lista foldLst t @?= [4]
                       
  ]
  ++
  [lista folda (binTree n n)  @?= replicate (2^n) n | n <- [1..10]]

p53 = test $
  [
    mapa folda id t                        @?= t,
    mapa folda next (Many (One 2) (One 3)) @?= Many (One 1) (One 10),
    mapa folda toUpper (binTree 10 'a')    @?= (binTree 10 'A'),

    -- alternative folds
    mapa foldFst (+1) t @?= One 2,
    mapa foldLst (+1) t @?= One 5
  ]

p54 = test $
  [
    -- p51 problems w/ folda'
    folda' id (*) (binTree 2 2) @?= 16,
    folda' (map toUpper) (++) (binTree 2 "a") @?= "AAAA",
    folda' show (++) t @?= "4321",

    -- p52
    suma folda' (binTree 10 1)                  @?= 2^10,
    longesta folda' (One "a")                   @?= 1,
    longesta folda' (Many (One "a") (One "bc")) @?= 2,
    longesta folda' (binTree 10 "asdf")         @?= 4,
    lista folda' t                              @?= lista folda t',
    lista folda' t                              @?= [4, 3, 2, 1],
    
    -- p53
    mapa folda' id t                        @?= t',
    mapa folda' id (binTree 10 1)           @?= binTree 10 1,
    mapa folda' next (Many (One 2) (One 3)) @?= Many (One 10) (One 1)
  ]
  
argMap 1 = p51
argMap 2 = p52
argMap 3 = p53
argMap 4 = p54
argMap _ = test [p51, p52, p53, p54]

hd :: [a] -> Maybe a
hd (x : xs) = Just x
hd []       = Nothing

main :: IO ()
main = do
  args <- getArgs
  let
    tests = case read <$> (hd args) of
          Just x -> argMap x
          Nothing -> argMap 42
  results <- runTestTT tests
  if (errors results + failures results == 0)
    then
      exitWith ExitSuccess
    else
      exitWith (ExitFailure 1)
