1. Given a datatype
```haskell
data BigSmall
    = Big Bool
    | Small Bool
    deriving (Eq, Show)
```
What is the cardinality of this datatype?
Hint: We already know Bool’s cardinality. Show your work as demonstrated earlier.
Answer: 2 for BigSmall and each of the data-constructors have a bool which adds
2 to each data constructor. So its 4. See below all possible:
- Big True
- Big False
- Small True
- Small False

2. Given a datatype
```haskell
-- bring Int8 in scope
import Data.Int

data NumberOrBool
    = Numba Int8
    | BoolyBool Bool
    deriving (Eq, Show)

-- parentheses due to syntactic
-- collision between (-) minus
-- and the negate function
let myNumba = Numba (-128)
```
What is the cardinality of NumberOrBool?
Answer: 2 for Bool and 256 for Int8. So a cardinality of 258

What happens if you try to create a Numba with a numeric literal larger than 127?
And with a numeric literal smaller than (-128)?
