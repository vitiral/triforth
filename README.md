# triforth

> **WARNING:** Work in progress

Triforth is a type-forth implementation in basic assembly. It serves three purposes:
- To demonstreate TypeForth, a type-safe variant of forth that is (mostly) ASM
  compliant.
- To teach how to create a FORTH and better understand assembly.
- As the core programming langauge for civboot.org


## Notes on type-stack

triforth works by maintaining a typestack which allows for tracking and verifying
compile-time. It also implements snapshotting and block-backtracking to implement
type inference in IF SWITCH and WHILE.
