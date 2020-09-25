# triforth

> **WARNING:** Work in progress

Triforth is a typeforth implementation in basic assembly. It serves three purposes:
- To demonstreate TypeForth, a type-safe variant of forth that is (mostly) ASM
  compliant.
- To teach how to create a FORTH and better understand assembly.
- As the core systems programming langauge for civboot.org


## Notes on typestack

triforth works by maintaining a typestack which allows for tracking and
verifying types at compile-time. It also implements snapshotting and
block-backtracking to implement type inference in IF SWITCH and WHILE.

## License
Dual-licensed under [MIT](./LICENSE) OR the [UNLICENSE](./UNLICENSE), at your
discression.

It is the intent that this software is in the public domain to be modified in
any way with no warranties of any kind.

