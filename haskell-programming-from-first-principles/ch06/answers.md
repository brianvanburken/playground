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
Multiple choice
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

