
## Notes on typestack

triforth works by maintaining a typestack which allows for tracking and
verifying types at compile-time. It also implements snapshotting and
block-backtracking to implement type inference in IF SWITCH and WHILE.

# How does IMMEDIATE and `DOES>` interact?
I think it's somewhat simple actually, like many things in forth.

* IMMEDIATE changes the precedence bit on the previous word. That's it.
* `DOES>` modifies the `xt` of the word to alter it's behavior. The
  Word will do what DOES> says instead of the default DOCOL. But there's
  a twist -- if the word is IMMEDIATE then it APPENDS EXIT and then the
  "does-xt" of the word on the execution stack of the immediate word. So when
  the word is used, the compiled word doesn't have access to the "normal xt",
  which is immediate. It has access to the "does-xt", which is not immediate.

To demonstrate, the word will look like this:
```
link | name | immediate-infoFlags | immediate-xt (DOCAL) | (cont...)
immediate-code... does-xt , EXIT |  (cont...)
does-infoFlags | does-xt (DOCOL) | does-code... EXIT
```

The `xt , ` in the "IMMEDIATE-code" will push does-xt onto the currently
compiling word.

# Drop semantics
If I restrict what types can be dropped using drop (basic types and refs) then
I can require all values to be dropped, since the rstack must be clean and you
can only drop a type by passing to function that takes its non reference. If it
is also impossible to clone/copy such values then you MUST clean up after
yourself! Suddenly there are no memory leaks, although there might still
be a dangling reference (we aren't tracking lifetimes after all).

# Hex Words
These are _very_ important :D

- ba5eba11 f005ba11
- bedabb1e aCab005e DeadBea7 0Ea7Beef 00Defec8 deadBeef
- ca11ab1e f01dab1e 5ca1ab1e a0ddba11 b01dface ca55e77e b0a710ad

# Data Structure Ideas
A pyramid list is a "list" which has pointers to lists which increase in size
by power of 2. There are
* 1 item in the first index 2^0 = 1
* 1 items in the second index  2^1 = 2
* 2 items in the third  2^3 = 4
* 4 in the fourth
* 8 in the fifth
* etc, until the maximum container size (if there is one)

The idea is that if you have an index you can use the BSR Instruction to find
it's power of 2 and jump to the correct container of values. In addition,
growing the list costs O(1) and never requires moving values.

The primary benefits are:
- Growing the list always costs O(1), so you no longer need to grow the list
  size by double in order to maintain amatorized O(1) insertion. This helps
  in embedded situations where consequitive memory blocks are at a premium.
- Access and insert operations are still O(1) although require an extra pointer
  lookup. However, iteration implementations can avoid the extra pointer lookup
  on average.

