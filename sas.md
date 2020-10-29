# SAS - Stack ASsembly

SAS is the assembly that civboot architecture is targeting to build into a
physical CPU. Before the CPU is constructed it will be: 

- emulated in x86 and proably wasm
- programmed into an FPGA

SAS is an 8 bit stack-based assembly that can target 16bit or 32 bit code
addresses and 32 bit data address spaces. This allows small programs to be
written with only 16bit code pointers, significantly reducing the code size of
forth code compiled to SAS.

The stack size in SAS is configurable via a register flag. SAS can run in either
16bit or 32bit stack mode via a register flag. All architectures MUST support
16bit mode. In 16 bit mode:
- a cell is 16 bits
- return and data stack uses 16 bit cells
- code access (xfetch/xstore) uses 16 bits
- data access (fetch/store) uses 2 cells (32 bits)
- the "base" code pointer is held in a register. 16 bit code addresses are
  incremented against this pointer.

In 32 bit mode:
- a cell is 32 bits
- return and data stack uses 32 bit cells
- code and data access both use 1 cell (32 bits)

There are 4 defined registers:
- dsp: contains the current data-stack address.
- rsp: contains the current return-stack address.
- ip: instruction pointer, contains address of the next instruction to execute
- vip: virtual instruction pointer, used with vnext operation to speed up forth
  execution.

In addition there are the `dstack(min|max)` and `rstack(min|max)` for defining
those stacks.

Instructions:
- `1XXX XXXX`: put the literal value `XX XXXX` (7-bits) directly on the stack
- `01XX XXXX`: put the literal at `XX XXXX` (6-bits) **cells** forward-offset
  from this instruction on the data stack
- `001_ ____`: unused
- `000X XXXX`: ALU operation, see [sas.asm](./sas.asm)

ext operation: the ext operation performs different behavior depending on the
top stack item. Most operations are platform-dependent and typically involve
interacting with a register. The following must always be supported:
