# Chapter 1

## Intermission: Equivalence Exercises

1. `λxy.xz`\
a) `λxz.xz` => false, `y` is substituted with `z` and used in body. Example has a free variable called `z`. This one does not\
b) `λmn.mz` => true, it is `alpha equivalent`\
c) `λz.(λx.xz)` => false, this does not have a free variable and is similair to choice _a_


2. `λxy.xxy`\
a) `λmn.mnp` => false, it has a free variable and the example not. Valid would be if it is `λmn.mmn`\
b) `λx.(λy.xy)` => false, body is missing a `x`\
c) `λa.(λb.aab)` => true, it is `alpha equivalent`. The expression has also expanded currying

3. `λxyz.zx`\
a) `λx.(λy.(λz.z))` => false, it is missing `x` in the body\
b) `λtos.st` => true, it is `alpha equivalent`\
c) `λmnp.mn` => false, it equal `λmnp.pm` to be valid


## Chapter Exercises

**Combinators** Determine if each of the following are combinators or not.\
1 `λx.xxx` => true, has no free variables\
2.`λxy.zx` => false, has a free variable `z`\
3.`λxyz.xy(zx)` => true, has no free variable\
4.`λxyz.xy(zxy)` => true, has no free variable\
5.`λxy.xy(zxy)` => false, has a free variable `z`

**Normal form or diverge?** Determine if each of the following can be reduced to a normal form or if they diverge.\
1.`λx.xxx` => normal\
2.`(λz.zz)(λy.yy)` => diverge, each variable in head is substituted with function and reduces to `(λy.yy)(λy.yy)` which can't be reduced any further\
3.`(λx.xxx)z` => normal

**Beta reduce** Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.\
1. `(λabc.cba)zz(λwv.w)` => original where `z`, `z`, and `(λw.λv.w)` are the arguments\
`(λa.λb.λc.cba)(z)z(λw.λv.w)` => make currying visible\
`(λb.λc.cbz)(z)(λw.λv.w)` => substitute `a` with `z` as argument\
`(λc.czz)(λw.λv.w)` => substitute `b` with `z` as argument\
`(λw.λv.w)(z)z` => substitute `c` with `(λw.λv.w)` as argument (since `c` is called with `zz` with the expression `czz`)\
`(λv.z)(z) z` => substitute `w` with `z` as argument\
`z` => since the body returns `z` we can reduce it further to `z`

2. `(λx.λy.xyy)(λa.a)b` => original with two argument `(λa.a)` and `b` \
`(λy.(λa.a)yy)b` => substitute `x` with first argument\
`(λa.a)(b)b` => substitute `y` with `b`
`bb` => since `a` is just returned we can reduce the body of two `a`'s to two `b`'s

3. `(λy.y)(λx.xx)(λz.zq)`

4. `(λz.z)(λz.zz)(λz.zy)` Hint: alpha equivalence

5. `(λx.λy.xyy)(λy.y)y`

6. `(λa.aa)(λb.ba)c`

7. `(λxyz.xz(yz))(λx.z)(λx.a)`
