# Chapter 3

## Exercises: Scope

1. These lines of code are from a REPL session. Is 𝑦 in scope for 𝑧?\
```
Prelude> let x = 5
Prelude> let y = 7`
Prelude> let z = x * y
```
Answer:\
Yes, it because it is define in the second line

2. These lines of code are from a REPL session. Is h in scope for 𝑔? Go with your gut here.\
```
Prelude> let f = 3
Prelude> let g = 6 * f + h
```
Answer:\
No, `h` is not defined here so it won't be found

3. This code sample is from a source file. Is everything we need to execute area in scope?\
```haskell
area d = pi * (r * r)
r = d / 2
```
Answer:\
Yes, in a file the definition doesn't have to be before it's usage

4. This code is also from a source file. Now are 𝑟 and 𝑑 in scope for area?
```haskell
area d = pi * (r * r)
  where r = d / 2
```
Answer:\
Yes, `r` is defined in the `where` and is scoped to the function. And `d` is a parameter so the `d` in the `where` is bound in the head.

## Exercises: Syntax Errors
Read the syntax of the following functions and decide whether it will compile. Test them in your REPL and try to fix the syntax errors where they occur.\
1. `++ [1, 2, 3] [4, 5, 6]` => Does not compile. The error here is in `++`. This is an infix operator if you want to use it as prefix you need to add parenthesis like so: `(++)`
2. `'<3' ++ ' Haskell'` => Does not compile. The single quotes indicate a single character. Here it used for a list of characters. It should be double quotes
3. `concat ["<3", " Haskell"]` => Does compile.


