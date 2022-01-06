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

## Chapter Exercises

### Parenthesization
Given what we know about the precedence of (*), (+), and (^), how can we parenthesize the following expressions more explicitly with- out changing their results? Put together an answer you think is correct, then test in the GHCi REPL.
For example, we want to make this more explicit\
\
`2 + 2 * 3 - 3`\
This will produce the same result:\
`2 + (2 * 3) - 3`\
Attempt the above on the following expressions:\

1. `2 + 2 * 3 - 1` => `((2 + (2 * 3)) - 1)` => `7`
2. `(^) 10 $ 1 + 1` => `(((^) 10) (1 + 1))` => `100`
3. `2 ^ 2 * 4 ^ 5 + 1` => `(((2 ^ 2) * (4 ^ 5)) + 1)` => `4097`

### Equivalent expressions
Which of the following pairs of expressions will return the same result when evaluated? Try to reason them out by reading the code and then enter them into the REPL to check your work:

1. `1 + 1 = 2` => true, 1 added to 1 results into 2
2. `10 ^ 2 = 10 + 9 * 10` => false, exponent means `10*10`. The right hand says `19*10`
3. `400 - 37 = (-) 37 400` => true, both use substraction. Difference is left hand has infix while the right hand has prefix operator
4. `100 \`div\` 3 = 100 / 3` => false, left hand uses integral division and rounds down while the right hand uses fractional division
5. `2 * 5 + 18 = 2 * (5 + 18)` => false, here the parenthesis override the higher precedence of multiplication

### More fun with functions
Here is a bit of code as it might be entered into a source file. Re- member that when you write code in a source file, the order is unim- portant, but when writing code directly into the REPL the order does matter. Given that, look at this code and rewrite it such that it could be evaluated in the REPL (remember: you may need let when entering it directly into the REPL). Be sure to enter your code into the REPL to make sure it evaluates correctly.

```haskell
z = 7
x = y ^ 2
waxOn = x * 5
y = z + 8
```
1. Now you have a value called waxOn in your REPL. What do you think will happen if you enter:\
\
`10 + waxOn` => this will add `10` to `waxOn` (which is `1125`) resulting in `1135`\
`(+10) waxOn` => this will add `10` to `waxOn` (which is `1125`) resulting in `1135`\
`(-) 15 waxOn` => this will subtract `waxOn` (which is `1125`) from `15` resulting in `-1110`\
`(-) waxOn 15` => this will subtract `15` from `waxOn` (which is `1125`) resulting in `1110`

2. Earlier we looked at a function called triple. While your REPL has waxOn in session, re-enter the triple function at the prompt:\
`let triple x = x * 3`

3. Now, what will happen if we enter this at our GHCi prompt? What do you think will happen first, considering what role waxOn is playing in this function call? Then enter it, see what does happen, and check your understanding:\
`triple waxOn` => this will triple the `waxOn` value. The `x` in `waxOn` is a free variable while the `x` in `triple` is bound. Calculus
`(\w.(* 3)w)(\a.(* 5)x)`

4. Rewrite waxOn as an expression with a where clause in your source file. Load it into your REPL and make sure it still works as expected.
```haskell
waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2
```

5. To the same source file where you have waxOn, add the triple function. Remember: You don’t need let and the function name should be at the left margin (that is, not nested as one of the waxOn expressions). Make sure it works by loading it into your REPL and then entering triple waxOn again at the REPL prompt. You should have the same answer as you did above.
6. Now, without changing what you’ve done so far in that file, add a new function called waxOff that looks like this: `waxOff x = triple x`
7.  Load the source file into your REPL and enter waxOff waxOn at the prompt.\
You now have a function, waxOff that can be applied to a variety of arguments — not just waxOn but any (numeric) value you want to put in for 𝑥. Play with that a bit. What is the result of waxOff 10 or waxOff (-50)? Try modifying your waxOff function to do something new — perhaps you want to first triple the 𝑥 value and then square it or divide it by 10. Spend some time getting comfortable with modifying the source file code, reloading it, and checking your modification in the REPL.\
`waxOff 10` => `30`, because it just triples the value
`waxOff (-50)` => `-150`, again because it triples the value
