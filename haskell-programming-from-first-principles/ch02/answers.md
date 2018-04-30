# Chapter 2

## Exercises: Comprehension Check
1. Given the following lines of code as they might appear in a source file, how would you change them to use them directly in the REPL?\
\
`half x = x / 2`\
Answer: `let half x = x / 2`, just add `let`\
\
`square x = x * x`\
Answer: `let square x = x * x`, just add `let`

2. Write one function that has one parameter and works for all the following expressions. Be sure to name the function.\
```
3.14 * (5 * 5)
3.14 * (10 * 10)
3.14 * (2 * 2)
3.14 * (4 * 4)
```
Answer:
```haskell
let circleArea width = 3.14 * (width * width)
```

3. There is a value in Prelude called pi. Rewrite your function to use pi instead of 3.14.
```haskell
let circleArea width = pi * (width * width)
```

## Exercises: Parentheses and Association
Below are some pairs of functions that are alike except for parenthe- sization. Read them carefully and decide if the parentheses change the results of the function. Check your work in GHCi.

1. \
a) `8 + 7 * 9`\
b) `(8 + 7) * 9`\
\
Answer: Yes, the parenthesis change the result.\
The difference is that at `a` it reduces down to `8 + 63`, while `b` reduces to `15 * 9`.\
Here the parenthesis overrules the higher precedence multiplication has over addition.

2. \
a) `perimeter x y = (x * 2) + (y * 2)`\
b) `perimeter x y = x * 2 + y * 2`\
\
Answer: No, due to the higher precedence of multiplication over addition the results will exactly be the same.

3. \
a) `f x = x / 2 + 9`\
b) `f x = x / (2 + 9)`\
Answer: Yes, the parenthesis change the result.\
The difference is that at `a` the variable `x` is divided first due to higher precedence while at `b` the addition is first evaluated.

## Exercises: Heal the Sick
The following code samples are broken and won’t compile. The first two are as you might enter into the REPL; the third is from a source file. Find the mistakes and fix them so that they will.

1. `let area x = 3. 14 * (x * x)` => space between `3.` and `1`. Fixed: `let area x = 3.14 * (x * x)`
2. `let double x = b * 2` => the variable `x` is not used and instead a random variable `b` appears in the body; `b` should be `x`. Fixed: `let double x = x * 2`
3. 
```haskell
x = 7
 y = 10
f = x + y
```
Anwer: problem is that `y` is indented incorrectly. It should be on the same level as `x`.
Fixed:
```haskell
x = 7
y = 10
f = x + y
```

## Exercises: A Head Code
Now for some exercises. First, determine in your head what the following expressions will return, then validate in the REPL:

1. `let x = 5 in x` => `5`
2. `let x = 5 in x * x` => `25`
3. `let x = 5; y = 6 in x * y` => `30`
4. `let x = 3; y = 1000 in x + 3` => `6`

Rewrite with where clauses:
1. `let x = 3; y = 1000 in x * 3 + y`\
Answer:
```haskell
answer1 = x * 3 + y
  where x = 3
        y = 1000
```
2. `let y = 10; x = 10 * 5 + y in x * 5`\
Answer:
```haskell
answer2 = x * 5
  where y = 10
        x = 10 * 5 + y
```
3. 
```haskell
let x = 7
    y = negate x
    z = y * 10
in z / x + y
```
Answer:
```haskell
answer3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10
```
