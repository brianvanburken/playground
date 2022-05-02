# Bottom Madness

Will the following expressions return a value or be ⊥?

1. `[x^y | x <- [1..5], y <- [2, undefined]]`
A: Throws an error for undefined because it returns everything to the REPL and therefore gets fully evaluated.

2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]`
A: This wil return the first item and doesn't evaluate the whole spine. Result is: `[1]`

3. `sum [1, undefined, 3]`
A: Throws an error because it evaluates the values to sum it. Undefined can't be added so it throws and error on the undefined.

4. `length [1, 2, undefined]`
A: Returns the length. None of the values are evaluated during `length`.

5. `length $ [1, 2, 3] ++ undefined`
A: Throws an error. Because we add `undefined` using `++` it gets evaluated.

6. `take 1 $ filter even [1, 2, 3, undefined]`
A: Because of `take 1` it does not evaluate all the values and just returns the first match. Result: `[2]

7. `take 1 $ filter even [1, 3, undefined]`
A: Throws an error. It needs to evaluate the whole list until it finds an even value to return. Because there are no even values the whole list get evaluated and it throws and error on the undefined.

8. `take 1 $ filter odd [1, 3, undefined]`
A: It returns the first element. It is the same case as question 6.

9. `take 2 $ filter odd [1, 3, undefined]`
A: Same as previous but this time it take the first 2 odd items. In this case 1 and 3 and thus undefined never gets evaluated.

10. `take 3 $ filter odd [1, 3, undefined]`
A: Throws and error. Here it needs to take 3 items. And the third item in the list is an undefined. Thus undefined gets evaluated and Haskell throws an error.
