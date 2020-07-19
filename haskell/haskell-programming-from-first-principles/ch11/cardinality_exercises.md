While we haven’t explicitly described the rules for calculating the cardinality of datatypes yet, you might already have an idea of how to do it for simple datatypes with nullary constructors. Try not to overthink these exercises — follow your intuition based on what you know.

1. data PugType = PugData
Answer: 1

2. For this one, recall that Bool is also defined with the |:
```haskell
data Airline
    = PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
```
Answer: 3

3. Given what we know about Int8, what’s the cardinality of Int16?
Answer: 65535

4. Use the REPL and maxBound and minBound to examine Int and Integer. What can you say about the cardinality of those types?
Answer: same as Int64

5. Extra credit (impress your friends!): What’s the connection between the 8 in Int8 and that type’s cardinality of 256?
Answer: the 8 is for the amount of bits used to store the number in memory.
Since a bit is a 1 or 0 (cardinality of 2) and can be repeated 8 times means its
limit is 2^8 which is 256.
