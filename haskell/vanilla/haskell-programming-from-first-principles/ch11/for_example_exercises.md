1. You can query the type of a value in GHCi with the :type command, also
abbreviated :t.
Example:
```haskell
Prelude> :t False
False :: Bool
```

```haskell
data Example = MakeExample deriving Show
```

What is the type of data constructor `MakeExample`?
Answer: `MakeExample :: Example`

What happens when you request the type of Example?
Answer: an error. You cant request the type of a type constructor

2. What if you try :info on Example in GHCi?
Can you determine what typeclass instances are defined for the Example type using
:info in GHCi?
Answer: using info you get more information over the type including lines which
tell which typeclasses it has. In this case it shows `Show`. Also it tells you
where it is defined.

3. Try making a new datatype like Example but with a single type argument added
to MakeExample, such as Int. What has changed when you query MakeExample with
:type in GHCi?
```haskell
data Example2 = MakeExample Int deriving Show
```
Using :type it shows that is a data constructor waiting for an Int input.

