
1. Will the following expression return a value or be ⊥?
`take 1 $ map (+1) [undefined, 2, 3]` => No, undefined is the first value. It throws an error on the undefined

2. Will the following expression return a value?
`take 1 $ map (+1) [1, undefined, 3]` => Yes, `2`. It only evaluates the first value of the list which is 1 and applies 1 to it and because of the
`take 1` it only evaluates the first item in the list.

3. Will the following expression return a value?
`take 2 $ map (+1) [1, undefined, 3]` => No, the undefined get evaluated and it returns an error.

4. What does the following mystery function do?
`itIsMystery xs = map (\x -> elem x "aeiou") xs`
A: It maps each item in the given list and checks for each item if the item is a vowel. It will return a list.

What is its type?
`itIsMystery :: [Char] -> [Bool]`

Describe it (to yourself or a loved one) in standard English and then test it out in the REPL to make sure you were correct.
Done!

5. What will be the result of the following functions:
a) `map (^2) [1..10]` => `[1,4,9,16,25,36,49,64,81,100][1,4,9,16,...100]`
b) `map minimum [[1..10], [10..20], [20..30]] -- n.b. 'minimum' is not the same function -- as the `min` that we used before` => `[1, 10, 20]`
c) `map sum [[1..5], [1..5], [1..5]]` => `[15,15,15]`

6. Back in chapter 7, you wrote a function called foldBool. That function exists in a module known as Data.Bool and is called bool. Write a function that does the same (or similar, if you wish) as the map (if-then-else) function you saw above but uses bool instead of the if-then-else syntax. Your first step should be bringing the bool function into scope by typing import Data.Bool at your Prelude prompt.

```haskell
import Data.Bool
map (\x -> bool x (-x) (x == 3))  [1..10]
```