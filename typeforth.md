# Typed Forth
> **WARN:** This is in thought experiment stage only.

It should be possible to add _types_ and _typechecking_ to forth with
zero runtime cost and _very little_ compiler and memory cost, while also
(mostly) adhering to the standard.

What is this magic? It all works in my head, lets see if it works on paper.

**Dictionary**: words in the dictionary have a few things added to them:
  - link: this is normal, it is the link to the previous dict entry.
  - name: this is normal, it is a padded counted string.
  - flags: 2 cells (64bits). This is normal but we add:
    - inplen: 3bit length of input types (up to 8)
    - outlen: 3bit length of output types (up to 8)
    - immediatelen: 8bit length of "immediate" code
    - moduleId: 10 bit module id. Allows for name overlap with `::` operator and
      USE.
    - linenum: 16 bit linenumber, with moduleId allows looking up the word with
      SEE.
  - the codeword/xt and immediate code of length immediatelen
  - inplen number of input xt's
  - outlen number of output xt's
  - localsize: the size needed for the local stack.

Datatypes like structs and enums don't have code they run, so the number of
types they support is inplen&outlen=6bits=64 and interfacelen is the size of
their interfaces (a list of xt's). After their interfaces, they have a vtable with
enough spaces for all the virtual methods (interfaces) they implement. Interfaces
are kind of like Java or Go interfaces or Rust traits.

When a typed-word is compiled into a word (i.e. `: foo myTypedWord ;`, it
inserts a command before and after it: reserveStack unreserveStack. These use
`localsize` to increase the local stack accordingly.

When declaring a word you can specify the type inside of `{ ... }`, which
can also specify local variables. Note that `{ ... }` is (mostly)
compliant with the ANS standard with one caveat. To be compliant, the output
types after `--` must be correct _and_ you must specify a type of `u` (or other
single-cell type) for all words that were previously unspecified, i.e.
`u:local1` instead of just `local1`, since type-forth will _not_ consume a
variable that has no name while ANS forth _will_. In other words { local1 ...
}` in TypeForth would be a value of type `local1` which is kept on the stack
a.k.a. not a local variable.  Items that are specified as 

Local type checking can be disabled if the first type is `nocheck:`, allowing
for writing internals in a simple forth as well as faster compatibility with
other forth code and optimizing code when necessary.

Types are specified in a few forms:
  - `: myFun { flag u:index &MyArray:arr | u:i -- d }` this is a fairly
    standard usage. This function accepts an unsigned cell (index), a MyArray
    _pointer_ (&) and a flag. It also has a local unsigned int named `i`.  Only
    the first two inputs and the local variable are named, they can be accessed
    by name within the function. The flag is left on the stack but is still
    part of the word signature. Note: "&" prefix specifies a reference.
  - `: myVFun { u:index &{ Vec+Debug }:v -- d }` this accepts the vref
    (virtual reference) `v` which must contain the `Vec` and `Debug`
    interfaces in it's vtable. All vrefs are represented as `( vt ref )` on the
    stack, with the vt being the address of the vtable and ref being a
    reference (pointer) to the data.
  - `: tswap { generic }`: the input and output are generic. The documentation
    for tswap is `( Ta Tb -- Tb Ta )`. It achieves this by programatically
    inspecting the typestack for the size of the top two types and writes this
    code into the function being compiled. It then alters the typestack
    appropriately as well. In typeforth, generics are a Simple Matter of
    Programming type manipulation. With it you can create generic map/filter/etc
    constructs, generic datatypes and many more things.
  - `: main { clear -- }`: `clear` means the function expects an empty stack.
  - `: clear { * -- }`: '*' is reserved for consuming the ENTIRE stack. Words of
    this type can only be used at the highest level or in functions that expect
    an empty stack (i.e. `{ clear -- ... }` or other functions that clear the
    stack.
  - `: myOptimizedFunction { nocheck: u -- u }`: the function is not
    type-checked, allowing for doing crazy things in LOOP/IF and using R> etc.
    You cannot use local variables in nocheck functions.
- When a word is compiled into the dictionary it encounters the `{ ... }`
  signature. It encodes the type indexes found into the tstack (type-stack, a
  separate stack or an arena-allocated memory area) and creates a checkpoint.
- While compiling a word, it keeps track of the type-stack by comparing and
  popping the xts of the inputs and pushing the xts of the outputs. It ensures
  that the values are kept consistent throughout.
  - Conceptually, when an xt is "compiled" into a word the compiler will also pop/push
    the _types_ onto the typestack -- throwing an error if things don't match.
- IF+ELSE+THEN or SWITCH statements are regarded as "block" and are handled thusly:
  - BLOCKSTART pushes the BLOCKSTART xt onto the block stack followed by a snapshot (copy)
    of the current type-stack.
  - It then compiles the types as normal, using the BLOCKSTART as the new bottom
    of the type-stack.
    - Note: If it encounters another BLOCKSTART then that block is checked
      first (by creating a new snapshot of the current stack) and it's
      input/output is used directly.
  - When it comes accross a BLOCKALT or BLOCKEND (i.e. ELSE, CASE, THEN, etc)
    it walks it's own stack backwards (the "return" stack of the block)
    comparing with the previous typestack. From this it infers the input/output
    types and encodes them.
      - This is rather simple actually. It starts at the bottom of both stacks
        and compares types until they don't match. These are discarded. The
        remaining types on the previous-stack are "input" types for the block
        and the remaining types on the block-stack are "output" types.
  - BLOCKALT do the _exact same procedure_. They clone the input stack etc. The
    difference is that when their types are inferred they compare them to the
    BLOCKSTART type signature.
- Most loops are not allowed to change the type-stack. BEGIN...AGAIN statements
  can return a value only within an internal block directly before a BREAK.
  - TODO: not sure how this is implemented, probably similar to above.

**Local values:** Local values are kept on the return stack, which is incremented
by the rstack-size before the word is EXECUTEd and the return addr put on the
stack. Usage of R> etc is _not permitted_ inside of type-checked functions for
this reason.  If use of R> is necessary, declare your function `nocheck`, which
does not allow local variables.

> Note: the return stack has a bitmap associated with it which tracks which
> addresses are actually return addresses and which are data. This is used
> by `R>` as well, enabling easier debugging.

**Structs:** structs are have a word-type of struct. They have no runtime
behavior, and at compiletime simply assert the state of the type-stack. They
store their input types as their fields -- their output type is their own xt.
They also store the interface xts and construct their vtable (described more
below).

```
STRUCT
  field type1 name1
  field type1 name2
  implements Interface1
  implements Interface2
END
```

**Methods:** a concrete method can be added to any struct by simply defining a
:METHOD which accepts a ref to the struct as it's last input, conventionally
named `self` (or unnamed). The method can then be called with `. myMethod`
(yes, we replace . -- use printu instead) `.` will lookup the method named
"myMethod" using the last value on the type-stack, which it will typecheck at
compile time and execute at runtime.

**Enums:** enumerations are rust-like and can contain both a variant and a
value. They are very similar to structs but have a few default methods and a
variantTable which is equal to inpSize&outSize.
- variantTable: contains numVariants cells after enumInfo, each cell contains
  the xt of method associated with that vi (variant index).
- `:METHOD vi { &MyEnum:self -- u }`: method to return the vi (variant index) which is
  an index into the variantTable.

Each variant is then encoded with it's own method used in ECASE. For example:
- `:method u { &MyEnum -- u } self @ ;`
- `:method d { &MyEnum -- d } 1 d@i ;` double at index 1 of address
- `:method empty { &MyEnum:self -- } ;`


Example use below. SWITCHE conceputally calls `dup $m vi SWITCH`. Each ECASE
will DROP the appropraite number of cells for the size of that variant relative
to the enum (the enum always takes up the same size on the stack).
```
myEnumValue SWITCHE
  ECASE u   # 1 cell dropped
    ... do stuff with u
  ENDCASE
  ECASE d   # no cells dropped
     ... do stuff with d
  ENDCASE
  ECASE empty # 2 cells dropped
    .... do stuff with nothing
  ENDCASE
THEN
```

Example definition with `( inline comments )`
```
ENUM
  VARIANT empty ( =name) EMPTY ( =type)
  VARIANT u ( =name)     u ( =type)
  VARIANT d ( =name)     d ( =type)
  IMPLEMENTS Debug
END
```

**VTable**: Space is reserved for the VTable when the Struct/Enum is
constructed and a perfect hashing function is dynamically created for the
struct to convert the xt of a virtual method into the appropriate method to
call. The xt of the perfect hashing function is stored in the first index
of the vTable.

To then assign methods to the type in question, use

```
IMPLEMENT Debug MyType
  dbg ( =Debug method) myDbg ( =your implementation)
END
```

**Modules** are implemented with a `MODULE myModuleName` which creates a dictionary
entry of type "module" and assigns it a new moduleId. Words defined after this
(including from other modules) will be of the associated module. If the module
is already defined, it will tell the interpreter to not execute the import.

`IMPORT path/to/myModule.fs` will import a module. the MODULE line will prevent
duplicate imports.

A word in a module can be looked up via `myModule . someWord`. FIND will only
find words with myModules's moduleId

`USE module1` will put the module in a global array so that FIND will find them
without a module-lookup. `USE clear` will clear this array, leaving only the
CORE module.

It should be noted that the following characters are not allowed in the name for
struct/enum/interface: `& ^ * @`
