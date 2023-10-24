# Leetcode 55 In 40 Characters Of BQN

Good ol' (https://leetcode.com/problems/jump-game/)[Leetcode 55], a classic in the interview circuit. At first glance, it's a simple question, just simulate the game, jump from the start index to each reachable index and see if we can reach the end. This leads to an O(n^2) solution and inevitably to the follow up question: "but can you do it in linear time?".

This proves deceptively difficult. Deceptive because the linear solution is (like most leetcode problems) simple. But if you haven't seen similair problems (or, you already know the "trick", in that case, shhh..) you're bound to waste valuable interview time teasing out the solution. But once it strikes you (or you look it up), you'll either kick yourself for missing something so obvious or pat yourself on the back for seeing through the interviewer's ruse.

Here's the idea:
Let x = array[0] and at each index (i) calculate x = max(x-1, array[i]). If x = 0 at or before the end of the array then we know we won't make it and we should return false.

Normal solutions are a couple lines of Python, but we can do it in just one line (40 characters!) of BQN (presumably, we could also do it in one line in most other array languages).

```
Jumpgame ← {a← 0⊑𝕩 , (×´ {a ≤ 𝕩 ? a↩𝕩; a↩a-1}¨ 𝕩) ≢ 0}
```

We can run this function as follows

```
Jumpgame ⟨0,2,1⟩
->		 0
Jumpgame ⟨ 2, 3, 1, 1, 4 ⟩
->		 1 
```

BQN is interepreted from right to left, but I'll walk through the code from right to left.

We define Jumpgame as a function which captures the value of the first index as the variable a, then we proceed to the second part of the function using the ',' seperator.

The inner function performs the algorithm discussed above. This results in a new array with the value max(x-1,array[i]) at each index i. By product folding this array (x') and comparing the result with 0, we get our answer - can we reach the end or not? 0 means that there's a 0 somewhere in the array, so, no we can't, 1 means yes we can reach the end.


