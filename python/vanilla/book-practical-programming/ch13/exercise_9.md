**Question:** There are at least two ways to come up with loop conditions. One of them is to answer the question, “When is the work done?” and then negate it. In function merge in ​Merging Two Sorted Lists​, the answer is, “When we run out of items in one of the two lists,” which is described by this expression: i1 == len(L1) or i2 == len(L2). Negating this leads to our condition i1 != len(L1) and i2 != len(L2).

Another way to come up with a loop condition is to ask, “What are the valid values of the loop index?” In function merge, the answer to this is 0 <= i1 < len(L1) and 0 <= i2 < len(L2); since i1 and i2 start at zero, we can drop the comparisons with zero, giving us i1 < len(L1) and i2 < len(L2).

Is there another way to do it? Have you tried both approaches? Which do you prefer?

**Answer:** Keep track of a flag. I prefer the way of "when is work done".