# Leetcode 55 In 32 Characters Of BQN

Good ol' [Leetcode 55](https://leetcode.com/problems/jump-game/), a classic in the interview circuit. At first glance, it's a simple question with a straightforward solution - just simulate the game, jump from the start index to each reachable index and see if we can reach the end. This leads to an O(n^2) solution and inevitably to the follow up question: "but can you do it in linear time?".

This proves deceptively difficult. Deceptive because the linear solution is (like most leetcode problems) simple. But unless you've seen similar problems (or, you already know the "trick") you're bound to waste valuable interview time teasing out the solution. But once the answer strikes you (or you look it up), you'll either kick yourself for missing something so obvious or pat yourself on the back for seeing through the interviewer's ruse.

Here's the idea, it's a basic dynamic programming solution:

Let x = array[0] and at each index (i) calculate x = max(x-1, array[i]).

If x == 0 at or before the end of the array then we know we won't make it and we should return false.

Following this recipe will get us an O(N) solution, which is normally ~10 lines of Python, but we can do it in just one line (32 characters!) of [BQN](https://mlochbaum.github.io/BQN/index.html) (presumably, we could also do it in one line in most other array languages, and Python list comprehensions will probably also get you close).

```
Jumpgame ← {a← 0⊑𝕩 , (×´ {a ≤ 𝕩 ? a↩𝕩; a↩a-1}¨ 𝕩) ≢ 0}

```

We can run this block as follows

```
Jumpgame ⟨0,2,1⟩
->		 0
Jumpgame ⟨ 2, 3, 1, 1, 4 ⟩
->		 1 
```

BQN doesn't exactly have the boolean value types True and False, instead they're represented naturally by 1 and 0.

BQN is interepreted from right to left, but I'll walk through the code from right to left.

```
Jumpgame ← {a← 0⊑𝕩 , 
```

We define Jumpgame as a block (which in this case, will behave as a function) that captures the value of the first index as the variable a, then we proceed to the second, inner block using the ',' seperator.

```
{a ≤ 𝕩 ? a↩𝕩; a↩a-1}¨ 𝕩
```

The inner block performs the algorithm discussed above. We use the previously defined variable a, and a variable that will be bound to the argument on the right side of the block, captured by 𝕩. The diaresis (¨) at the end of this block means "Each", resulting in the function being performed over each element , i.e., mapped over the given list. This results in a new array with the value max(x-1,array[i]) at each index i.

```
(×´ {a ≤ 𝕩 ? a↩𝕩; a↩a-1}¨ 𝕩) ≢ 0}
```

By product folding this array (x' means fold by multiplying each element the list with the next element) and comparing the result with 0, we get our answer - can we reach the end or not? 0 means that there's a 0 somewhere in the array, so, no we can't. 1 means yes we can reach the end.


Admittedly, you could probably code golf this down quite a bit further. But I think this solution is fine as is, and as a bonus, it's surprisingly readable for an array language!