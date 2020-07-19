1. Is Doggies a type constructor or a data constructor?
Answer: type constructor

2. What is the kind of `Doggies`?
Answer: `Doggies :: * -> *`

3. What is the kind of `Doggies String`?
Answer: `Doggies [Char] :: *`

4. What is the type of `Husky 10`?
Answer: `Num a => Doggies a`

5. What is the type of `Husky (10 :: Integer)`?
Answer: `Doggies Integer`

6. What is the type of `Mastiff "Scooby Doo"`?
Answer: `Doggies [Char]`

7. Is `DogueDeBordeaux` a type constructor or a data constructor?
Answer: type constructor

8. What is the type of `DogueDeBordeaux`?
Answer: `doge -> DogueDeBordeaux doge`

9. What is the type of `DogueDeBordeaux "doggie!"`?
Answer: `DogueDeBordeaux [Char]`
