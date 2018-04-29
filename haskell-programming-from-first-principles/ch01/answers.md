# Chapter 1

## Intermission: Equivalence Exercises

1. `\xy.xz`\
a) `\xz.xz` => false, `y` is substituted with `z` and used in body. Example has a free variable called `z`. This one does not\
b) `\mn.mz` => true, it is `alpha equivalent`\
c) `\z.(\x.xz)` => false, this does not have a free variable and is similair to choice _a_


2. `\xy.xxy`\
a) `\mn.mnp` => false, it has a free variable and the example not. Valid would be if it is `\mn.mmn`\
b) `\x.(\y.xy)` => false, body is missing a `x`\
c) `\a.(\b.aab)` => true, it is `alpha equivalent`. The expression has also expanded currying

3. `\xyz.zx`\
a) `\x.(\y.(\z.z))` => false, it is missing `x` in the body\
b) `\tos.st` => true, it is `alpha equivalent`\
c) `\mnp.mn` => false, it equal `\mnp.pm` to be valid


## Chapter Exercises

**Combinators** Determine if each of the following are combinators or not.\
1 `\x.xxx` => true, has no free variables\
2.`\xy.zx` => false, has a free variable `z`\
3.`\xyz.xy(zx)` => true, has no free variable\
4.`\xyz.xy(zxy)` => true, has no free variable\
5.`\xy.xy(zxy)` => false, has a free variable `z`

**Normal form or diverge?** Determine if each of the following can be reduced to a normal form or if they diverge.\
1.`\x.xxx` => normal\
2.`(\z.zz)(\y.yy)` => diverge, each variable in head is substituted with function and reduces to `(\y.yy)(\y.yy)` which can't be reduced any further\
3.`(\x.xxx)z` => normal

**Beta reduce** Evaluate (that is, beta reduce) each of the following expressions to normal form. We strongly recommend writing out the steps on paper with a pencil or pen.\
1. `(\abc.cba)zz(\wv.w)` => original where `z`, `z`, and `(\w.\v.w)` are the arguments\
`(\a.\b.\c.cba)zz(\w.\v.w)` => make currying visible\
`(\b.\c.cbz)z(\w.\v.w)` => substitute `a` with `z` as argument\
`(\c.czz)(\w.\v.w)` => substitute `b` with `z` as argument\
`(\w.\v.w)zz` => substitute `c` with `(\w.\v.w)` as argument (since `c` is called with `zz` with the expression `czz`)\
`(\v.z)zz` => substitute `w` with `z` as argument\
`z` => since the body returns `z` we can reduce it further to `z`

2. `(\x.\y.xyy)(\a.a)b` => original with two argument `(\a.a)` and `b` \
`(\y.(\a.a)yy)b` => substitute `x` with first argument\
`(\a.a)bb` => substitute `y` with `b`
`bb` => since `a` is just returned we can reduce the body of two `a`'s to two `b`'s

3. `(\y.y)(\x.xx)(\z.zq)` => original with two lambda expressions as arguments
`(\x.xx)(\z.zq)` => subtitude `y` in the body for the first argument
`(\z.zq)(\z.zq)` => subtitude `x` with the argument
`(\z.zq)(q)` => replace `z` with the argument
`qq` => replace the `z` with argument `q`

4. `(\z.z)(\z.zz)(\z.zy)` => original\
`(\z.zz)(\z.zy)` => subtitude argument\
`(\z.zy)(\z.zy)` => subtitude both body variables with the lambda expression passed as an argument\
`(\z.zy)y` => subtitude first lambda expression with it's evaluation (just returning the passed lambda expression and a free variable `y`\
`yy` => subtitude last lambda expression like previous step to end up with two `y`'s a free variable

5. `(\x.\y.xyy)(\y.y)y` => original\
`(\y.(\y.y)yy)y` => subtitude `x` with lambda expression passed as an argument\
`(\y.y)yy` => subtitude `y` in lambda expression body with `y` passed as an argument\
`yy` => evaluate last lambda expression to end up with two `y`'s

6. `(\a.aa)(\b.ba)c` => original\
`(\b.ba)(\b.ba)c` => subtitude both `a`'s with argument\
`(\b.ba)ac` => subtitude first lambda expresion\
`aac` => the last lambda expression take `a` and returns it with an extra `a`

7. `(\xyz.xz(yz))(\x.z)(\x.a)` => original\
`(\x.\y.\z.xz(yz))(\x.z)(\x.a)` => make currying visible\
`(\x.\y.\z.xzyz)(\x.z)(\x.a)` => remove extra parenthesis\
`(\y.\w.(\x.z)wyw)(\x.a)` => subtitude `x` with argument and rename `z` in head to `w` to distinquish since the other `z` which is a free variable\
`(\w.(\x.z)w(\x.a)w)` => subtitude `y` with argument\
`(\w.z(\x.a)w)` => subtitude `(\x.z)w` with `z`. The lambda expression takes an argument but does nothing with it. That's why we lose `w` and `z` is a free variable so that one stays\
`(\w.za)` => same as previous step. We lose the `w` and get a free variable `a`
