# Chapter 8

## Chapter Exercises

1.  What is the type of `[[True, False], [True, True], [False, True]]`?
    a) `Bool`\
    b) mostly `True`\
    c) `[a]`\
    d) `[[Bool]]` => True

2.  Which of the following has the same type as `[[True, False], [True, True], [False, True]]`?
    a) `[(True, False), (True, True), (False, True)]`
    b) `[[3 == 3], [6 > 5], [3 < 4]]` => True
    c) `[3 == 3, 6 > 5, 3 < 4]`
    d) `["Bool", "more Bool", "Booly Bool!"]`

3.  For the following function

```haskell
func :: [a] -> [a] -> [a]
func x y = x ++ y
```

which of the following is true?
a) x and y must be of the same type
b) x and y must both be lists
c) if x is a String then y must be a String
d) all of the above => True

4.  For the func code above, which is a valid application of func to both of its arguments?
    a) func "Hello World"
    b) func "Hello" "World" => True
    c) func [1, 2, 3] "a, b, c"
    d) func ["Hello", "World"]

## Reviewing currying

Given the following definitions, tell us what value results from further applications.

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
```

    1. What is the value of appedCatty "woohoo!" ? => "woops mrow woohoo!"
    2. frappe "1" => "1 morw haha"
    3. frappe (appedCatty "2") => "woops morw 2 morw haha"
    4. appedCatty (frappe "blue") => "woops morw blue morw haha"
    5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) => "pink morw haha mrow green morw woops morw blue"
    6. cattyConny (flippy "Pugs" "are") "awesome" => "are mrow Pugs mrow awesome"

## Recursion

1.  Write out the steps for reducing dividedBy 15 2 to its final answer according to the Haskell code.

```
dividedBy 15 2 =
    go 15 2 0
    go (15 - 2) 2 (0 + 1)
    go (13 - 2) 2 (1 + 1)
    go (11 - 2) 2 (2 + 1)
    go (09 - 2) 2 (3 + 1)
    go (07 - 2) 2 (4 + 1)
    go (05 - 2) 2 (5 + 1)
    go (03 - 2) 2 (6 + 1)
    go 1 2 7
    | 1 < 2 = (7, 1)
```

2.  Write a function that recursively sums all numbers from 1 to n, n being the argument. So that if n was 5, you’d add 1+2+3+4+5 to get 15. The type should be (Eq a, Num a) => a -> a. See `sum.hs`
3.  Write a function that multiplies two integral numbers using recursive summation. The type should be (Integral a) => a -> a -> a. See `multiplication.hs`

## Fixing dividedBy

Our dividedBy function wasn’t quite ideal. For one thing. It was a partial function and doesn’t return a result (bottom) when given a divisor that is 0 or less.
Using the pre-existing div function we can see how negative numbers should be handled:

```
 Prelude> div 10 2
 5
 Prelude> div 10 (-2)
-5
 Prelude> div (-10) (-2)
 5
 Prelude> div (-10) (2)
-5
```

The next issue is how to handle zero. Zero is undefined for division in math, so we ought to use a datatype that lets us say there was no sensible result when the user divides by zero. If you need inspiration, consider using the following datatype to handle this.

```haskell
data DividedResult
    = Result Integer
    | DividedByZero
```

See `dividedBy.hs`

## McCarthy 91 function

See `mc91.hs`

## Numbers into words

See `numbersIntoWords.hs`
