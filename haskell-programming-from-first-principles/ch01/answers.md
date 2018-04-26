# Chapter 1

## Intermission: Equivalence Exercises

1.`𝜆𝑥𝑦.𝑥𝑧`
a) `𝜆𝑥𝑧.𝑥𝑧` => false, `y` is replaced with `z` and used in body. Example has a free variable. This one not.
b) `𝜆𝑚𝑛.𝑚𝑧` => true, it is `alpha equivalent`
c) `𝜆𝑧.(𝜆𝑥.𝑥𝑧)` => false, this does not have a free variable and is similair to choice a


2.`𝜆𝑥𝑦.𝑥𝑥𝑦`
a) `𝜆𝑚𝑛.𝑚𝑛𝑝` => false, it has a free variable and the example not. Valid would be if it is `𝜆𝑚𝑛.𝑚𝑚𝑛`
b) `𝜆𝑥.(𝜆𝑦.𝑥𝑦)` => false, body is missing a `𝑥`
c) `𝜆𝑎.(𝜆𝑏.𝑎𝑎𝑏)` => true, it is `alpha equivalent`. The expression has also expanded currying

3. `𝜆𝑥𝑦𝑧.𝑧𝑥`
a) `𝜆𝑥.(𝜆𝑦.(𝜆𝑧.𝑧))` => false, it is missing `𝑥` in the body
b) `𝜆𝑡𝑜𝑠.𝑠𝑡` => true, it is `alpha equivalent`
c) `𝜆𝑚𝑛𝑝.𝑚𝑛` => false, it equal `𝜆𝑚𝑛𝑝.𝑝𝑚` to be valid


## Chapter Exercises

**Combinators** Determine if each of the following are combinators or not.
1 `𝜆𝑥.𝑥𝑥𝑥` => true, has no free variables
2.`𝜆𝑥𝑦.𝑧𝑥` => false, has a free variable `𝑧`
3.`𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥)` => true, has no free variable
4.`𝜆𝑥𝑦𝑧.𝑥𝑦(𝑧𝑥𝑦)` => true, has no free variable
5.`𝜆𝑥𝑦.𝑥𝑦(𝑧𝑥𝑦)` => false, has a free variable `𝑧`

**Normal form or diverge?** Determine if each of the following can be reduced to a normal form or if they diverge.
1.`𝜆𝑥.𝑥𝑥𝑥` => normal
2.`(𝜆𝑧.𝑧𝑧)(𝜆𝑦.𝑦𝑦)` => diverge, each variable in head is replace with function and reduces to `(𝜆𝑦.𝑦𝑦)(𝜆𝑦.𝑦𝑦)`
3.`(𝜆𝑥.𝑥𝑥𝑥)𝑧` => normal

**Beta reduce** Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.
1. `(𝜆𝑎𝑏𝑐.𝑐𝑏𝑎)𝑧𝑧(𝜆𝑤𝑣.𝑤)` original where `z`, `z`, and `(𝜆𝑤.𝜆𝑣.𝑤)` are the arguments
`(𝜆𝑎.𝜆𝑏.𝜆𝑐.𝑐𝑏𝑎)(𝑧)𝑧(𝜆𝑤.𝜆𝑣.𝑤)` make currying visible
`(𝜆𝑏.𝜆𝑐.𝑐𝑏𝑧)(𝑧)(𝜆𝑤.𝜆𝑣.𝑤)` replace `a` with `z` as argument
`(𝜆𝑐.𝑐𝑧𝑧)(𝜆𝑤.𝜆𝑣.𝑤)` replace `b` with `z` as argument
`(𝜆𝑤.𝜆𝑣.𝑤)(𝑧)𝑧` replace `c` with `(𝜆𝑤.𝜆𝑣.𝑤)` as argument (since `c` is called with `zz` with the expression `czz`)
`(𝜆𝑣.𝑧)(𝑧) 𝑧` replace `w` with `z` as argument
`z` since the body returns `z` we can reduce it further to `z`
