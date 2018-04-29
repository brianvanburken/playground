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
