# Chapter 4

## Exercises: Mood Swing
Given the following datatype, answer the following questions:
```haskell
data Mood = Blah | Woot deriving Show
```
1. What is the type constructor, or name of this type?\
Answer: `Mood`

2. If the function requires a Mood value, what are the values you could possibly use?\
Answer: `Blah` or `Woot`

3. We are trying to write a function changeMood to change Chris’s mood instantaneously. It should act like not in that, given one value, it returns the other value of the same type. So far, we’ve written a type signature changeMood :: Mood -> Woot. What’s wrong with that?\
Answer: it should return type `Mood`. It now returns a value of `Mood` and if it is suppose to flip then we won't now the value, since it's based on the value given.

4. Now we want to write the function that changes his mood. Given an input mood, it gives us the other one. Fix any mistakes and complete the function:\
```haskell
changeMood Mood = Woot
changeMood    _ = Blah
```
We’re doing something here called pattern matching. We can define a function by matching on a data constructor, or value, and describing the behavior that the function should have based on which value it matches. The underscore in the second line denotes a catch-all, otherwise case. So, in the first line of the function, we’re telling it what to do in the case of a specific input. In the second one, we’re telling it what to do regardless of all potential inputs. It’s trivial when there are only two potential values of a given type, but as we deal with more complex cases, it can be convenient.\
\
Answer:
```haskell
changeMood Blah = Woot
changeMood _ = Blah
```

5. Enter all of the above — datatype (including the deriving Show bit), your corrected type signature, and the corrected function into a source file. Load it and run it in GHCi to make sure you got it right.\
Answer: see `mood.hs`

## Exercises: Find the Mistakes

The following lines of code may have mistakes — some of them won’t compile! You know what you need to do.

1. `not True && true` => broken. Second `true` should be capitalized
2. `not (x = 6)` => broken. The comparison should be double equals `==`
3. `(1 * 2) > 5` => works. Result: `False`
4. `[Merry] > [Happy]` => works? If `Happy` and `Merry` are of the same data
5. `[1, 2, 3] ++ "look at me!"` => broken. Can't join list and string. They're not of the same type

## Chapter Exercises

As in previous chapters, you will gain more by working out the answer before you check what GHCi tells you, but be sure to use your REPL to check your answers to the following exercises. Also, you will need to have the awesome, also, and allAwesome code from above in scope for this REPL session. For convenience of reference, here are those values again:\
```haskell
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]
```
length is a function that takes a list and returns a result that tells how many items are in the list.

1. Given the definition of length above,\
what would the type signature be? Answer: `length: [a] -> Int`\
How many arguments, of what type does it take? Answer: one argument. Type is a `list` of anything, so `[a]`\
What is the type of the result it evaluates to? Answer: `Int` because a length is represented as a number


2. What are the results of the following expressions?\
a) `length [1, 2, 3, 4, 5]` => `5`
b) `length [(1, 2), (2, 3), (3, 4)]` =>  `3`
c) `length allAwesome` => `2`
d) `length (concat allAwesome)` => `5`

3. Given what we know about numeric types and the type signature of length, look at these two expressions. One works and one returns an error. Determine which will return an error and why.\
(n.b., you will find Foldable t => t a representing [a], as with concat in the previous chapter. Again, consider Foldable t to represent a list here, even though list is only one of the possible types.)\
`Prelude> 6 / 3` => works. This wil result in `2.0`\
`Prelude> 6 / length [1, 2, 3]` => broken. Type of right-hand is an `Int` but is not part of `Fractional` that is needed for `/`

4. How can you fix the broken code from the preceding exercise using a different division function/operator?\
Answer: use `div`

5. What is the type of the expression 2 + 3 == 5? What would we expect as a result?\
Answer: `Bool` with value `True`

6. What is the type and expected result value of the following:\
```
Prelude> let x = 5
Prelude> x + 3 == 5
```
Answer: `Bool` with value `False`

7. Below are some bits of code. Which will work? Why or why not? If they will work, what value would these reduce to?\
`Prelude> length allAwesome == 2` => works. The result of the `length` expression is `2` which equals to the `2` on the right hand side. So result is: `True`\
`Prelude> length [1, 'a', 3, 'b']` => broken. List should have all of the same type. In this case it's a mix of integers and characters\
`Prelude> length allAwesome + length awesome` => works. Both `length` expresions will return an `Int` which can be added using `+`. Result: `5`\
`Prelude> (8 == 8) && ('b' < 'a')` => works. Both expressions are valid to be reduced. Result will be false since the right hand side of the `&&` is `False` and the AND operator requires both side to be `True` for a truthy result\
`Prelude> (8 == 8) && 9` => broken. Right side of AND operator (`&&`) is not an expression that results in `Bool`. It is instead an `Int`

8. Write a function that tells you whether or not a given String (or list) is a palindrome. Here you’ll want to use a function called reverse a predefined function that does what it sounds like.\
Answer:
```haskell
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =
    x == reverse x
```

9. Write a function to return the absolute value of a number using if-then-else\
```haskell
myAbs :: Integer -> Integer
myAbs n =
    if n < 0 then
        n * (-1)
    else
        n

```

10. Fill in the definition of the following function, using fst and snd:\
```haskell
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 =
    (,) (snd t1, snd t2) (fst t1, fst t2)
```

### Correcting syntax

In the following examples, you’ll be shown syntactically incorrect code. Type it in and try to correct it in your text editor, validating it with GHC or GHCi.

1. Here, we want a function that adds 1 to the length of a string argument and returns that result.\
```haskell
x = (+)

F xs = w 'x' 1
    where w = length xs
```
Answer: `F` should be lowercase `f` because function names should start with lowercase

2. This is supposed to be the identity function, id.\
```haskell
\X = x
```
Answer: `\X` should be lowercase `x`

3. When fixed, this function will return 1 from the value (1, 2).\
```haskell
f (a b) = A
```
Answer: `A` should be lowercase `a` to refer to `a` in the tuple pattern matching

### Match the function names to their types
1. Which of the following types is the type of show?\
a) `show a => a -> String` => this one\
b) `Show a -> a -> String`\
c) `Show a => a -> String`

2. Which of the following types is the type of (==)?\
a) `a -> a -> Bool`\
b) `Eq a => a -> a -> Bool` => this one\
c) `Eq a -> a -> a -> Bool`\
d) `Eq a => A -> Bool`

3. Which of the following types is the type of fst?\
a) `(a, b) -> a` => this one\
b) `b -> a`\
c) `(a, b) -> b`

4. Which of the following types is the type of (+)?\
a) `(+) :: Num a -> a -> a -> Bool`\
b) `(+) :: Num a => a -> a -> Bool`\
c) `(+) :: num a => a -> a -> a`\
d) `(+) :: Num a => a -> a -> a` => this one\
e) `(+) :: a -> a -> a`
