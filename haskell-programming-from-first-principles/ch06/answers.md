# Chapter 6

## Exercises: Eq Instances
Write the Eq instance for the datatype provided.

1. It’s not a typo, we’re just being cute with the name.
```haskell
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn x') = x == x'
```

2.
```haskell
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x y) (Two x' y') = x == x' && y == y'
```

3.
```haskell
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt x') = x == x'
    (==) (TisAString y) (TisAString y') = y == y'
    (==) _ _ = False
```

4.
```haskell
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair x y) (Pair x' y') = x == x' && y == y'
```

5.
```haskell
data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'
```

6.
```haskell
data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) (ThatOne x) (ThisOne x') = x == x'
    (==) (ThisOne x) (ThatOne x') = x == x'
```

7.
```haskell
data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye x) (Goodbye x') = x == x'
    (==) _ _ = False
```

## Exercises: Will They Work?
Next, take a look at the following code examples and try to decide if they will work, what result they will return if they do, and why or why not (be sure, as always, to test them in your REPL once you have decided on your answer):

1. `max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])` => True, both length return `Int` which implements `Ord` typeclass that is needed for `max`\
2. `compare (3 * 4) (3 * 5)` => True, both have the `Ord` typeclass implemented. Thus it works\
3. `compare "Julie" True` => False, not the same concreet type. Though both do implement `Ord`, they're both different
4. `(5 + 3) > (3 + 6)` => True, both implement `Ord` needed for `(>)`

## Chapter Exercises

### Multiple choice

1. The Eq class
a) includes all types in Haskell => False, IO is not comparable\
b) is the same as the Ord class => False, Ord is ordering and that is different to comparing\
c) makes equality tests possible => True, Eq stands for equality\
d) only includes numeric types => False, String also has Eq

2. The typeclass Ord
a) allows any two values to be compared => True, you need to compare to say which one is lesser or greater than for ordering\
b) is a subclass of Eq => True, you need equality to compare\
c) is a superclass of Eq => False, is a subclass\
d) has no instance for Bool => False, Ord is implemented for Bool

3. Suppose the typeclass Ord has an operator >. What is the type of >?
a) `Ord a => a -> a -> Bool` => True\
b) `Ord a => Int -> Bool` => False\
c) `Ord a => a -> Char` => False\
d) `Ord a => Char -> [Char]` => False

4. In `x = divMod 16 12`
a) the type of 𝑥 is Integer => False, we don't know the concreet type of 16 or 12\
b) the value of 𝑥 is undecidable => False, it returns a tuple\
c) the type of 𝑥 is a tuple => True, with the first item the result and second the remainder\
d) `𝑥` is equal to `12 / 16` => False, `(/)` does not return a tuple

5. The typeclass Integral includes
a) Int and Integer numbers => True\
b) integral, real, and fractional numbers => False, not fractional\
c) Schrodinger’s cat => True && False (joke) it is False\
d) only positive numbers => False

### Does it typecheck?
1. Does the following code typecheck? If not, why not?
```haskell
data Person = Person Bool

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
```
Fails, because Person has not derived from Show. It can be fixed by adding `deriving (Show)` to `Person`.

2. Does the following typecheck? If not, why not?
```haskell
 data Mood = Blah | Woot deriving Show

settleDown x =
    if x == Woot
    then Blah
    else x
```
Fails, because `settleDown` uses equality on `Mood` value and this fails because `Mood` does not derive from `Eq`.
Add `Eq` to the `deriving` part to fix it.

3. If you were able to get settleDown to typecheck:\
a) What values are acceptable inputs to that function?\
Only `Mood` values so `Blah` or `Woot`\
b) What will happen if you try to run settleDown 9? Why?\
Fails, because it is not a value of `Mood`\
c) What will happen if you try to run Blah > Woot? Why?\
Fails, because it needs `Ord` implemented and `Mood` does not have that

4. Does the following typecheck? If not, why not?
```haskell
type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
```
It does. Everything is sound

### Given a datatype declaration, what can we do?
Given the following datatype definitions:
```haskell
data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)
```

Which of the following will typecheck? For the ones that don’t
typecheck, why don’t they?
1. phew = Papu "chases" True\
Does not work. Each value needs to be wrapped in its own Type constructor so: `phew = Papu (Rocks "chases") (Yeah True)`

2. truth = Papu (Rocks "chomskydoz") (Yeah True)\
Works, because all values are wrapped correctly

3. `equalityForall :: Papu -> Papu -> Bool; equalityForall p p' = p == p'`\
Works, because `Papu` derives from `Eq` so they are comparable

4. `comparePapus :: Papu -> Papu -> Bool; comparePapus p p' = p > p'`\
Fails, because `Papu` does not derive from `Ord` which is needed for `(>)`

### Match the types
1. For the following definition.
a) `i :: Num a => a; i = 1`
b) Try replacing the type signature with the following: `i :: a`\
After you’ve formulated your own answer, then tested that answer and believe you understand why you were right or wrong, make sure to use GHCi to check what type GHC infers for the definitions we provide without a type assigned. For example, for this one, you’d type in:\
```
Prelude> let i = 1
Prelude> :t i
-- Result elided intentionally.
```
Answer: you must add `Num a` constaint. I guessed it was possible due to polymorphish but it's not

2.\
a) f :: Float; f = 1.0\
b) f :: Num a => a\
Answer: Fails, because `Num a` is to general

3.\
a) f :: Float; f = 1.0\
b) f :: Fractional a => a\
Answer: Works, because it the most polymorphic type it is more specific

4.\
a) f :: Float; f = 1.0\
b) f :: RealFrac a => a\
Answer: Works, it is even more specific than the case in question 3

5.\
a) freud :: a -> a; freud x = x\
b) freud :: Ord a => a -> a\
Answer: Works, the typeclass can be added but it is ignored

6.\
a) freud' :: a -> a; freud' x = x\
b) freud' :: Int -> Int\
Answer: Works, it is more specific with the concreet types

7.\
a)\
```haskell
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX
```
b) sigmund :: a -> a\
Answer: Fails, because we go from a concreet type to polymorphic and the concreet type is enforce by `myX`

8.\
a)\
```haskell
myX = 1 :: Int

sigmund' :: Int -> Int
sigmund' x = myX
```
b) sigmund' :: Num a => a -> a\
Answer: Fails, `myX` still enforces the concreet type

9.\
a) You’ll need to import sort from Data.List.\
```haskell
jung :: Ord a => [a] -> a
jung xs = head (sort xs)
```
b) jung :: [Int] -> Int\
Answer: Works, it is more specific and Int implements Ord

10.\
a) young :: [Char] -> Char; young xs = head (sort xs)\
b) young :: Ord a => [a] -> a\
Answer: Works, not enforces the concreet type here. Only the `sort` needs the `Ord`. As long as the value passed in
implements `Ord` everything is fine.

11.\
a)\
```haskell
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)
```
b) signifier :: Ord a => [a] -> a\
Answer: Fails, because `mySort` enforces `Char` so the new type is to generic

### Type-Kwon-Do Two: Electric Typealoo
1.\
```haskell
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk = ???
```
Answer:\
```haskell
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk xToY x y =
    (xToY x) == y
```
2.\
```haskell
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith = ???
```
Answer:\
```haskell
arith :: Num b => (a -> b) -> Integer -> a -> b
arith xToY y x =
    (xToY x) + (fromInteger y)
```
