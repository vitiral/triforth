
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
