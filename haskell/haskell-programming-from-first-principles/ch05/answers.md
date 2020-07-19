# Chapter 5

## Exercises: Type Matching
Below you’ll find a list of several standard functions we’ve talked about
previously. Under that is a list of their type signatures. Match the function to
its type signature. Try to do it without peeking at the type signatures (either
in the text or in GHCi) and then check your work. You may find it easier to
start from the types and work out what you think a function of that type would do.


1. Functions:
a) c `not :: Bool -> Bool`
b) d `length :: [a] -> Int`
c) b `concat :: [[a]] -> [a]`
d) a `head :: [a] -> a`
e) e `(<) :: Ord a => a -> a -> Bool`

## Exercises: Type Arguments

Given a function and its type, tell us what type results from applying some or all of the arguments.
You can check your work in the REPL like this (using the first question as an example):
```haskell
Prelude> let f :: a -> a -> a -> a; f = undefined
Prelude> let x :: Char; x = undefined
Prelude> :t f x
```
It turns out that you can check the types of things that aren’t implemented yet, so long as you give GHCi an undefined to bind the signature to.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `𝑥` is `Char` then the
typeof `f x` is\
a) `Char -> Char -> Char` => True, all of `a` are a `Char` so it kan be replaced\
b) `x -> x -> x -> x` => False, `a` has a defined type\
c) `a -> a -> a` => False, this does not have the right amount of arguments\
d) `a -> a -> a -> Char` => False, this will always return `Char` while the example return type depends on given type

2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"` is\
a) `String` => False, the type of the second argument is the return type and the second argument is not a String\
b) `Char -> String` => False, all arguments are provided so there is no partial function returned\
c) `Int` => False, same problem as with option a\
d) `Char` => True, the same type as the second argument `b`

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1.0 2` is:\
a) `Double` => False, none of the arguments have a specific type\
b) `Integer` => False, same problem as option a\
c) `Integral b => b` => False, `Integral` is more specific than `Num` and since none of the argument have this type the statement is false\
d) `Num b => b` => True, as defined by the type class constraint all are `Num` and since non have a stricter type

Note that because the type variables 𝑎 and 𝑏 are different, the compiler must assume that the types could be different.

4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1 (5.5 :: Double)` is\
a) `Integer` => False, the stricter type of `b` set is `Double` and since `b` is the return type, and all arguments are given, this statement is false\
b) `Fractional b => b` => False, this type is not set on the arguments or as type class constraint\
c) `Double` => True, since `b` is a `Double`, `b` is also the return type and all arguments are given, the return type is returned\
d) `Num b => b` => True

5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard" "has the word jackal in it"`\
a) `[Char]` => True\
b) `Eq b => b` => False\
c) `b -> [Char]` => False\
d) `b` => False\
e) `Eq b => b -> [Char]` => False

6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of `jackal "keyboard"`\
a) `b` => False\
b) `Eq b => b` => False\
c) `[Char]` => False\
d) `b -> [Char]` => False\
e) `Eq b => b -> [Char]` => True\

7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 2` is\
a) `Integer` => False\
b) `Int` => False\
c) `a` => False\
d) `(Num a, Ord a) => a` => True\
e) `Ord a => a` => False\
f) `Num a => a` => False

8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel 1 (2 :: Integer)` is\
a) `(Num a, Ord a) => a` => True\
b) `Int` => False\
c) `a` => False\
d) `Num a => a` => False\
e) `Ord a => a` => False\
f) `Integer` => False\

9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of `kessel (1 :: Integer) 2` is\
a) `Num a => a` => False\
b) `Ord a => a` => False\
c) `Integer` => True, type is set for `a` so the return type will be the same. Since all arguments are given the return type is returned\
d) `(Num a, Ord a) => a` => False\
e) `a` => False

## Exercises: Parametricity
All you can do with a parametrically polymorphic value is pass or not pass it to some other expression. Prove that to yourself with these small demonstrations.

1. Given the type a -> a, which is the type for id, attempt to make a function that terminates successfully that does something other than returning the same value. This is impossible, but you should try it anyway.\
Answer: impossible

2. We can get a more comfortable appreciation of parametricity by looking at a -> a -> a. This hypothetical function a -> a -> a has two–and only two–implementations. Write both possi- ble versions of a -> a -> a. After doing so, try to violate the constraints of parametrically polymorphic values we outlined above.\
Answer: `f x y = x` and `f x y = y`

3. Implement a -> b -> b. How many implementations can it have? Does the behavior change when the types of 𝑎 and 𝑏 change?\
Answer: `f x y = y` thus only one implementation. If both were `Num` we could do more like addition. But we know nothing about the types so we can only return

## Exercises: Apply Yourself
Look at these pairs of functions. One function is unapplied, so the compiler will infer maximally polymorphic type. The second function has been applied to a value, so the inferred type signature may have become concrete, or at least less polymorphic. Figure out how the type would change and why, make a note of what you think the new inferred type would be and then check your work in GHCi.

1.
```haskell
-- Type signature of general function
(++) :: [a] -> [a] -> [a]
-- How might that change when we apply
-- it to the following value?
myConcat x = x ++ " yo"
```
Answer: change because myConcat has already a value filled in from which the type\
could be inferred. So `myConcat :: [Char] -> [Char]`

2.
```haskell
-- General function
(*) :: Num a => a -> a -> a
-- Applied to a value
myMult x = (x / 3) * 5
```
Answer: because `(/)` works with factorials the `(*)` becomes more specific

3.
```haskell
take :: Int -> [a] -> [a]
myTake x = take x "hey you"
```
Answer: since there is no partial application the whole take is return with `a` filled\
in. The `a` is replaced with `Char` since `String` is a list of chars.

4.
```haskell
(>) :: Ord a => a -> a -> Bool
myCom x = x > (length [1..10])
```
Answer: here `a` becomes an `Int` because `length` returns an `Int`. So the signature becomes `myCom :: Int -> Bool`

5.
```haskell
(<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z'
```
Answer: same case as with question 4. Since the right hand side is filled with the char `z` it wil resolve to `myAlph :: Char -> Bool` because `a` must be the same for `(<)`

## Chapter Exercises
Welcome to another round of “Knowing is not enough; we must apply.”

Multiple choice
1. A value of type [a] is\
a) a list of alphabetic characters => False, `a` could be anything!\
b) a list of lists => False, `a` could be anything since the type is not known\
c) a list whose elements are all of some type 𝑎 => True\
d) a list whose elements are all of different types => False, all must be the same type

2. A function of type [[a]] -> [a] could\
a) take a list of strings as an argument => True, a string is a list of chars so `a` could be subtituted with `Char` \
b) transform a character into a string => False, then it would only take `a` and place it in a list. So: `a -> [a]`\
c) transform a string into a list of strings => False\
d) take two arguments => False, it only takes one argument. A list of list with the same type

3. A function of type [a] -> Int -> a\
a) takes one argument => False, it takes two argument. List of `a` and an `Int`\
b) returns one element of type 𝑎 from a list => True\
c) must return an Int value => False, it returns anything that has the same type as `a`. So `Int` is possible but not a must\
d) is completely fictional => False, the type for `a` is open so it could be anything

4. A function of type (a, b) -> a\
a) takes a list argument and returns a Char value => False, there is no explicit type defined. And first argument is a tuple not a list\
b) has zero arguments => False, it has one argument a tuple with two values\
c) takes a tuple argument and returns the first value => True, it receives a tuple and since the first `a` is also returned this is possible\
d) requires that 𝑎 and 𝑏 be of different types => False, both could be different but also the same

### Determine the type
For the following functions, determine the type of the specified value. We suggest you type them into a file and load the contents of the file in GHCi. In all likelihood, it initially will not have the polymorphic types you might expect due to the monomorphism restriction. That means that top-level declarations by default will have a concrete type if any can be determined. You can fix this by setting up your file like so:
```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where
-- simple example
example = 1
````
If you had not included the NoMonomorphismRestriction extension, example would have had the type Integer instead of Num a => a. Do your best to determine the most polymorphic type an expression could have in the following exercises.

1. All function applications return a value. Determine the value returned by these function applications and the type of that value.\
a) `(* 9) 6` => `54 :: Num a => a -> a`\
b) `head [(0,"doge"),(1,"kitteh")]` => `(0, "doge") :: Num a => (a, [Char])`\
c) `head [(0 :: Integer ,"doge"),(1,"kitteh")]` => `(0, "doge") :: (Integer, [Char])`\
d) `if False then True else False` => `False :: Bool`\
e) `length [1, 2, 3, 4, 5]` => `5 :: Int`\
f) `(length [1, 2, 3, 4]) > (length "TACOCAT")` => `False :: Bool`\

2. Given
```haskell
x = 5
y = x + 5
w = y * 10
```
What is the type of `w`? `Num a => a`

3. Given
```haskell
x = 5
y = x + 5
z y = y * 10
```
What is the type of `z`? `Num a => a -> a`

4. Given
```haskell
x = 5
y = x + 5
f = 4 / y
```
What is the type of `f`? `Factorial a => a`

5. Given
```haskell
x = "Julie"
y = " <3 "
z = "Haskell"
f = x ++ y ++ z
```
What is the type of f? `[Char]`

## Does it compile?

For each set of expressions, figure out which expression, if any, causes the compiler to squawk at you (n.b. we do not mean literal squawking) and why. Fix it if you can.

1. ```haskell
bigNum = (^) 5 $ 10
wahoo = bigNum $ 10
```

Does not compile because bigNum is already a solved expression. Applying 10 to it does not work.
To fix it you can pass bigNum to a arithmetic function like (+) before passing in the 10

2. ```haskell
x = print
y = print "woohoo!"
z = x "hello world"
```
Compiles.

3. ```haskell
a = (+)
b = 5
c = b 10
d = c 200
```
Does not compile. Since the expression at `c` applies 10 to 5 as a value. Values cannot be applied to values, only to functions.
To fix it you can replace `b` in `c = b 10` with `a`.

4. ```haskell
a = 12 + b
b = 10000 * c
```
Does not compile. `c` does not exist and thus it fails. To fix it you need to define `c`.

## Type variable or specific type constructor?
1. You will be shown a type declaration, and you should categorize each type. The choices are a fully polymorphic type variable, constrained polymorphic type variable, or concrete type con- structor.
```haskell
f :: Num a => a -> b -> Int -> Int
--           [0]  [1]   [2]    [3]
```
Here, the answer would be: constrained polymorphic (Num) ([0]), fully polymorphic ([1]), and concrete ([2] and [3]).


2. Categorize each component of the type signature as described in the previous example.
```haskell
f :: zed -> Zed -> Blah
--   (a)    (b)    (c)
```
a) fully polymorphic
b) concreet
c) concreet

3. Categorize each component of the type signature
```
f :: Enum b => a -> b -> C
--            (a)  (b)  (c)
```
a) fully polymorphic
b) constrained polymorphic
c) concreet

4. Categorize each component of the type signature
```
f :: f -> g -> C
--  (a)  (b)  (c)
```
a) fully polymorphic
b) fully polymorphic
c) concreet

## Write a type signature

For the following expressions, please add a type signature. You should be able to rely on GHCi type inference to check your work, although you might not have precisely the same answer as GHCi gives (due to polymorphism, etc).

1. While we haven’t fully explained this syntax yet, you’ve seen it in Chapter 2 and as a solution to an exercise in Chapter 4. This syntax is a way of destructuring a single element of a list by pattern matching.
```haskell
functionH :: [a] -> a
functionH (x:_) = x
```

2. ```haskell
functionC :: Ord a => a -> a -> Bool
functionC x y =
    if (x > y) then True else False
```

3. ```haskell
functionS :: (a, b) -> b
functionS (x, y) = y
```

## Given a type, write the function
1. ```haskell
i :: a -> a
i x = x
```

2. ```haskell
c :: a -> b -> a
c x _ = x
```

3.
```haskell
c'' :: b -> a -> b
c'' x _ = x
````

4. ```haskell
c' :: a -> b -> b
c' _ y = y
```

5. ```haskell
r :: [a] -> [a]
r x = x
```

6. ```haskell
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = (bToC (aToB x))
```

7. ```haskell
a :: (a -> c) -> a -> a
a _ x = x
```

8. ```haskell
a' :: (a -> b) -> a -> b
a' aToB = aToB
```

## Fix it

1. and 2. ```haskell
module sing where

fstString :: [Char] -> [Char] -- replaced ++ with ->
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char] -- wrap Char with []
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y -- replace else with or and flip condition
    where x = "Singin" -- Fix indent
          y = "Somewhere" -- Rename x to y
```

3. ```haskell
module Arith3Broken where

main :: IO ()
main = do -- lowercase m
    print (1 + 2) -- wrap expression with parenthesis
    putStrLn "10" -- convert 10 to a string
    print (negate (-1)) -- wrap -1 with parenthesis to make it negative and not partial application
    print ((+) 0 blah)
        where blah = negate 1
```

## Type-Kwon-Do

1. ```haskell
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h x = (g (f x))
```

2. ```haskell
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e x = (w (q x))
```

3. ```haskell
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = ((xz x), (yz y))
```

4. ```haskell
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge xToY yToWz x = fst $ yToWz $ xToY x
```
