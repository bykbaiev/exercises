{- |
Module                  : Lecture2
Copyright               : (c) 2021-2022 Haskell Beginners 2022 Course
SPDX-License-Identifier : MPL-2.0
Maintainer              : Haskell Beginners 2022 Course <haskell.beginners2022@gmail.com>
Stability               : Stable
Portability             : Portable

Exercises for the Lecture 2 of the Haskell Beginners course.

As in the previous section, implement functions and provide type
signatures. If the type signature is not already written, write the
most polymorphic type signature you can.

Unlike exercises to Lecture 1, this module also contains more
challenging exercises. You don't need to solve them to finish the
course but you can if you like challenges :)
-}

module Lecture2
    ( -- * Normal
      lazyProduct
    , duplicate
    , removeAt
    , evenLists
    , dropSpaces

    , Knight (..)
    , dragonFight

      -- * Hard
    , isIncreasing
    , merge
    , mergeSort

    , Expr (..)
    , Variables
    , EvalError (..)
    , eval
    , constantFolding
    ) where
import Data.Maybe

-- VVV If you need to import libraries, do it after this line ... VVV

-- ^^^ and before this line. Otherwise the test suite might fail  ^^^

{- | Implement a function that finds a product of all the numbers in
the list. But implement a lazier version of this function: if you see
zero, you can stop calculating product and return 0 immediately.

>>> lazyProduct [4, 3, 7]
84
-}
lazyProduct :: [Int] -> Int
lazyProduct = go 1
  where go :: Int -> [Int] -> Int
        go res [] = res
        go _ (0:_) = 0
        go res (x:xs) = go (res * x) xs

{- | Implement a function that duplicates every element in the list.

>>> duplicate [3, 1, 2]
[3,3,1,1,2,2]
>>> duplicate "cab"
"ccaabb"
-}
duplicate :: [a] -> [a]
duplicate = go []
  where go :: [a] -> [a] -> [a]
        go xs [] = xs
        go xs (y:ys) = go (xs ++ [y, y]) ys

{- | Implement function that takes index and a list and removes the
element at the given position. Additionally, this function should also
return the removed element.

>>> removeAt 0 [1 .. 5]
(Just 1,[2,3,4,5])

>>> removeAt 10 [1 .. 5]
(Nothing,[1,2,3,4,5])
-}
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt n = go 0 []
  where go :: Int -> [a] -> [a] -> (Maybe a, [a])
        go _ filtered [] = (Nothing, filtered)
        go index filtered (x:xs)
          | index == n = (Just x, filtered ++ xs)
          | otherwise = go (index + 1) (filtered ++ [x]) xs

{- | Write a function that takes a list of lists and returns only
lists of even lengths.

>>> evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
[[3,1,2,7],[]]

♫ NOTE: Use eta-reduction and function composition (the dot (.) operator)
  in this function.
-}
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

{- | The @dropSpaces@ function takes a string containing a single word
or number surrounded by spaces and removes all leading and trailing
spaces.

>>> dropSpaces "   hello  "
"hello"
>>> dropSpaces "-200            "
"-200"

♫ NOTE: As in the previous task, use eta-reduction and function
  composition (the dot (.) operator) in this function.

🕯 HINT: look into Data.Char and Prelude modules for functions you may use.
-}
dropSpaces :: String -> String
dropSpaces = filter (/= ' ')

{- |

The next task requires to create several data types and functions to
model the given situation.

An evil dragon attacked a village of innocent citizens! After
returning to its lair, the dragon became hungry and ate one of its
treasure chests by accident.

The guild in the village found a brave knight to slay the dragon!
As a reward, the knight can take the treasure chest.

Below is the description of the fight and character specifications:

  * A chest contains a non-zero amount of gold and a possible treasure.
    When defining the type of a treasure chest, you don't know what
    treasures it stores insight, so your chest data type must be able
    to contain any possible treasure.
  * As a reward, knight takes all the gold, the treasure and experience.
  * Experience is calculated based on the dragon type. A dragon can be
    either red, black or green.
  * Red dragons grant 100 experience points, black dragons — 150, and green — 250.
  * Stomachs of green dragons contain extreme acid and they melt any
    treasure except gold. So green dragons has only gold as reward.
    All other dragons always contain treasure in addition to gold.
  * Knight tries to slay dragon with their sword. Each sword strike
    decreases dragon health by the "sword attack" amount. When the
    dragon health becomes zero or less, a dragon dies and the knight
    takes the reward.
  * After each 10 sword strikes, dragon breathes fire and decreases
    knight health by the amount of "dragon fire power". If the
    knight's health becomes 0 or less, the knight dies.
  * Additionally, each sword strike decreases "knight's endurance" by one.
    If knight's endurance becomes zero, they become tired and are not
    able to continue the fight so they run away.

Implement data types to describe treasure, knight and dragon.
And implement a function that takes a knight and a dragon and returns
one of the three possible fight outcomes.

You're free to define any helper functions.

🕯 HINT: If you find the description overwhelming to implement entirely
  from scratch, try modelling the problem in stages.

    1. Implement all custom data types without using polymorphism.
    2. Add @newtype@s for safety where you think is appropriate.
    3. Encode the fight result as a sum type.
    4. Add polymorphism.
    5. Make all invalid states unrepresentable. Think, how you can
       change your types to prevent green dragons from having any
       treasure besides gold (if you already haven't done this).
-}

-- some help in the beginning ;)

newtype Health = Health Int

mkHealth :: Int -> Maybe Health
mkHealth value
  | value < 0 = Nothing
  | otherwise = Just $ Health value

unHealth :: Health -> Int
unHealth (Health health) = health

newtype AttackPower = AttackPower Int

mkAttackPower :: Int -> Maybe AttackPower
mkAttackPower value
  | value < 0 = Nothing
  | otherwise = Just $ AttackPower value

unAttackPower :: AttackPower -> Int
unAttackPower (AttackPower power) = power

newtype Endurance = Endurance Int

mkEndurance :: Int -> Maybe Endurance
mkEndurance value
  | value < 0 = Nothing
  | otherwise = Just $ Endurance value

unEndurance :: Endurance -> Int
unEndurance (Endurance endurance) = endurance

data Treasure = Treasure

newtype Gold = Gold Int

mkGold :: Int -> Maybe Gold
mkGold value
  | value < 0 = Nothing
  | otherwise = Just $ Gold value

unGold :: Gold -> Int
unGold (Gold gold) = gold

newtype Experience = Experience Int

mkExperience :: Int -> Maybe Experience
mkExperience value
  | value < 0 = Nothing
  | otherwise = Just $ Experience value

unExperience :: Experience -> Int
unExperience (Experience experience) = experience

data Knight = Knight
    { knightHealth    :: Health
    , knightAttack    :: AttackPower
    , knightEndurance :: Endurance
    }

data KnightStats = KnightStats { ksH :: Int
                               , ksP :: Int
                               , ksE :: Int }

mkKnight :: KnightStats -> Maybe Knight
mkKnight stats = case (mkHealth $ ksH stats, mkAttackPower $ ksP stats, mkEndurance $ ksE stats) of
  (Just h, Just p, Just e) -> Just $ Knight { knightHealth    = h
                                            , knightAttack    = p
                                            , knightEndurance = e }
  _                        -> Nothing

data DragonInfo = DragonInfo
  { dragonHealth    :: Health
  , dragonFirePower :: AttackPower
  , dragonGold      :: Gold}

data DragonInfoStats = DragonInfoStats { disH :: Int
                                       , disP :: Int
                                       , disG :: Int }

mkDragonInfo :: DragonInfoStats -> Maybe DragonInfo
mkDragonInfo stats = case (mkHealth $ disH stats, mkAttackPower $ disP stats, mkGold $ disG stats) of
  (Just h, Just p, Just g) -> Just $ DragonInfo { dragonHealth     = h
                                                , dragonFirePower = p
                                                , dragonGold      = g}
  _                        -> Nothing

unDragonInfo :: Dragon -> DragonInfo
unDragonInfo (Red _ info)   = info
unDragonInfo (Black _ info) = info
unDragonInfo (Green info)   = info

data Dragon
  = Red (Maybe Treasure) DragonInfo
  | Black (Maybe Treasure) DragonInfo
  | Green DragonInfo

dragonExperience :: Dragon -> Experience
dragonExperience (Red _ _)   = fromMaybe (Experience 0) $ mkExperience 100
dragonExperience (Black _ _) = fromMaybe (Experience 0) $ mkExperience 150
dragonExperience (Green _)   = fromMaybe (Experience 0) $ mkExperience 250

dragonTreasure :: Dragon -> Maybe Treasure
dragonTreasure (Red treasure _) = treasure
dragonTreasure (Black treasure _) = treasure
dragonTreasure (Green _) = Nothing

data TreasuresChest = ChestWithTreasure (Maybe Treasure) | EmptyChest

data WinPerks = WinPerks TreasuresChest Gold Experience

mkWinPerks :: Dragon -> WinPerks
mkWinPerks (Green info) = WinPerks EmptyChest (dragonGold info) (dragonExperience (Green info))
mkWinPerks dragon       = WinPerks (ChestWithTreasure $ dragonTreasure dragon)
                                   (dragonGold $ unDragonInfo dragon)
                                   (dragonExperience dragon)

data FightResults = Win WinPerks | Loss

isDragonDead :: Dragon -> Bool
isDragonDead = (== 0) . unHealth . dragonHealth . unDragonInfo

isKnightDead :: Knight -> Bool
isKnightDead = (== 0) . unHealth . knightHealth

isKnightWeak :: Knight -> Bool
isKnightWeak = (== 0) . unEndurance . knightEndurance

hitKnight :: AttackPower -> Knight -> Knight
hitKnight power knight = knight { knightHealth = Health (max health 0) }
  where health = (unHealth $ knightHealth knight) - (unAttackPower power)

weakenKnight :: Knight -> Knight
weakenKnight knight = knight { knightEndurance = Endurance endurance }
  where endurance = (unEndurance $ knightEndurance knight) - 1

hitDragon :: AttackPower -> Dragon -> Dragon
hitDragon power dragon = case dragon of
                          Red treasure info   -> Red treasure updatedInfo
                          Black treasure info -> Black treasure updatedInfo
                          Green info          -> Green updatedInfo
  where info        = unDragonInfo dragon
        health      = (unHealth $ dragonHealth info) - 1
        updatedInfo = info { dragonHealth = Health (max 0 health) }

dragonFight :: Knight -> Dragon -> FightResults
dragonFight = go 0
  where go :: Int -> Knight -> Dragon -> FightResults
        go counter knight dragon 
          | isDragonDead dragon = Win $ mkWinPerks dragon
          | isKnightDead knight = Loss
          | isKnightWeak knight = Loss
          | counter == 10       = go 0
                                     (weakenKnight $ hitKnight (dragonFirePower $ unDragonInfo dragon) knight)
                                     (hitDragon (knightAttack knight) dragon)
          | otherwise = go (counter + 1) (weakenKnight knight) (hitDragon (knightAttack knight) dragon)

----------------------------------------------------------------------------
-- Extra Challenges
----------------------------------------------------------------------------

{- The following exercises are considered optional. Some of them might be more
challenging. However, you still may find some of them easier than some of the
previous ones. Difficulty is a relative concept.
-}

{- | Write a function that takes a list of numbers and returns 'True'
if all the numbers are in the increasing order (i.e. the list is
sorted).

>>> isIncreasing [3, 1, 2]
False
>>> isIncreasing [1 .. 10]
True
-}
isIncreasing :: [Int] -> Bool
isIncreasing = go Nothing [] 
  where go :: Maybe Int -> [Int] -> [Int] -> Bool 
        go _ _ [] = True
        go Nothing xs (y:ys) = go (Just y) xs ys
        go (Just x) xs (y:ys)
          | x > y = False
          | otherwise = go (Just y) (xs ++ [x]) ys


{- | Implement a function that takes two lists, sorted in the
increasing order, and merges them into new list, also sorted in the
increasing order.

The lists are guaranteed to be given sorted, so you don't need to
verify that.

>>> merge [1, 2, 4] [3, 7]
[1,2,3,4,7]
-}
merge :: [Int] -> [Int] -> [Int]
merge = go []
  where go :: [Int] -> [Int] -> [Int] -> [Int]
        go res xs [] = res ++ xs
        go res [] ys = res ++ ys
        go res (x:xs) (y:ys)
          | x == y    = go (res ++ [x, y]) xs ys
          | x < y     = go (res ++ [x]) xs (y:ys)
          | otherwise = go (res ++ [y]) (x:xs) ys

{- | Implement the "Merge Sort" algorithm in Haskell. The @mergeSort@
function takes a list of numbers and returns a new list containing the
same numbers but in the increasing order.

The algorithm of merge sort is the following:

  1. If the given list has less than 2 elements, it's already sorted.
  2. Otherwise, split list into two lists of the same size.
  3. Sort each of two lists recursively.
  4. Merge two resulting sorted lists to get a new sorted list.

>>> mergeSort [3, 1, 2]
[1,2,3]
-}
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = (go . splitAt ((length list + 1) `div` 2)) list
  where go :: ([Int], [Int]) -> [Int]
        go (xs, ys) = merge (mergeSort xs) (mergeSort ys)


{- | Haskell is famous for being a superb language for implementing
compilers and interpeters to other programming languages. In the next
tasks, you need to implement a tiny part of a compiler.

We're going to work on a small subset of arithmetic operations.

In programming we write expressions like "x + 1" or "y + x + 10".
Such expressions can be represented in a more structured way (than a
string) using the following recursive Algebraic Data Type:
-}
data Expr
    = Lit Int
    | Var String
    | Add Expr Expr
    deriving (Show, Eq)

{- Now, you can use this data type to describe such expressions:

> x + 1
Add (Var "x") (Lit 1)

> y + x + 10
Add (Var "y") (Add (Var "x") (Lit 10))
-}

{- | We want to evaluate such expressions. We can associate a value
with a variable using a list of pairs.

You can use the @lookup@ function to search in this list by a variable name:

 * https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:lookup
-}
type Variables = [(String, Int)]

{- | Unfortunately, it's not guaranteed that variables in our @Expr@
data type are present in the given list. So we're going to introduce a
separate data for possible evaluation errors.

Normally, this would be a sum type with several constructors
describing all possible errors. But we have only one error in our
evaluation process.
-}
data EvalError
    = VariableNotFound String
    deriving (Show, Eq)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

calculateAllLiterals :: Expr -> (Int, [String])
calculateAllLiterals = go (0, [])
  where go :: (Int, [String]) -> Expr -> (Int, [String])
        go (value, variables) (Lit x) = (value + x, variables)
        go (value, variables) (Var x) = (value, variables ++ [x])
        go (value, variables) (Add augend addend) = (value + firstValue + secondValue
                                                    , variables ++ firstVariables ++ secondVariables)
          where (firstValue, firstVariables) = go (0, []) augend
                (secondValue, secondVariables) = go (0, []) addend

{- | Having all this set up, we can finally implement an evaluation function.
It returns either a successful evaluation result or an error.
-}
eval :: Variables -> Expr -> Either EvalError Int
eval variables expression = getSumOfVars value vars
  where (value, vars) = calculateAllLiterals expression
        getSumOfVars :: Int -> [String] -> Either EvalError Int
        getSumOfVars res [] = Right res
        getSumOfVars res (var:vs) = if isNothing varValue
                                    then Left $ VariableNotFound var
                                    else getSumOfVars (res + fromMaybe 0 varValue) vs
          where varValue :: Maybe Int
                varValue = fmap snd $ maybeHead $ filter (\(n, _) -> n == var) variables

{- | Compilers also perform optimizations! One of the most common
optimizations is "Constant Folding". It performs arithmetic operations
on all constants known during compile time. This way you can write
more verbose and clear code that works as efficient as its shorter
version.

For example, if you have an expression:

x + 10 + y + 15 + 20

The result of constant folding can be:

x + y + 45

It also can be:

x + 45 + y

Write a function that takes and expression and performs "Constant
Folding" optimization on the given expression.
-}
constantFolding :: Expr -> Expr
constantFolding expr = if null variables
                       then Lit res
                       else removeZero $ Add (Lit res) (varsToExpr variables)
  where (res, variables) = calculateAllLiterals expr
        varsToExpr :: [String] -> Expr
        varsToExpr = foldr1 Add . map Var
        removeZero :: Expr -> Expr
        removeZero (Add (Lit 0) expr) = expr
        removeZero (Add expr (Lit 0)) = expr
        removeZero expr = expr
