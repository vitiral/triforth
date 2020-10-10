\ Now that we have defined our core interpreter in triforth.S we
\ are ready to define the rest of the language.
\
\ Please note that we don't have ( comments ) until we define them,
\ so we will use line comments for everything.

&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode, nostack
: /       /mod swap drop ; \ ( a b -- a/b )
: mod     /mod drop ;   \ ( a b -- a%b )
: '\n'    0x A ;
'\n' 0x A assertEq assertEmpty      \ test: '\n' works, nostack
&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode, nostack

: bl      0x 20 ;       \ BL (Blank) is std forth word for space
: cr      '\n' emit ;   \ print carriage return a.k.a newline
: space   bl emit ;     \ print space
: negate  0x 0 SWAP - ; \ get negative of the number
&latest nt>xt dumpInfo drop &latest 0x 48 dump
: LITERAL IMM  \ ( u -- ) take whatever is on stack and compile as a literal
  ' LIT , \ compile xt of LIT
  , ;     \ compile the literal itself (from the stack)
: ':' \ ( -- c) the colon character code
  [ CHAR : ]  \ put the character ':' onto the stack at compile-time
  LITERAL ;   \ compile LIT 58 as the definition for ':' word
&state @ 0x 0 assertEq          \ test: we are in runmode
':' 0x 3A assertEq assertEmpty  \ test: ':' works, therefore LITERAL, etc works

: ';' [ CHAR ; ] LITERAL ; \ ;
: '(' [ CHAR ( ] LITERAL ; \ (
: ')' [ CHAR ) ] LITERAL ; \ )
: '"' [ CHAR " ] LITERAL ; \ "
: 'A' [ CHAR A ] LITERAL ; \ A
: '0' [ CHAR 0 ] LITERAL ; \ 0
: '-' [ CHAR - ] LITERAL ; \ -
: '.' [ CHAR . ] LITERAL ; \ .
';' 0x 3B assertEq  '(' 0x 28 assertEq  ')' 0x 29 assertEq \ test: yo'basic

: VARIABLE WORD create drop ; \ Allow defining variables

VARIABLE testCache 0x 0 , 0x 0 , 0x 0 , \ temp storage for tests

\ Addressing and indexing mechanisms
: cell   0x 4 ;        \ ( -- u ) the cell size
: cell+  cell + ;  \ ( u -- u ) increment by cell size
: cells  cell * ;  \ ( u -- u ) multiply by cell size
: @i     cells + @ ; \ ( addr i -- u ) fetch at index=i of addr
: @1     cell+ @ ; \ ( addr -- u ) fetch at index=1 of addr
: @2     [ 0x 2 CELL * ] LITERAL + @ ; \ ( addr -- u ) fetch at index=1 of addr
: @3     [ 0x 3 CELL * ] LITERAL + @ ; \ ( addr -- u ) fetch at index=1 of addr
: !i     cells + ! ; \ ( u addr i -- ) store u at index=i of addr
: !1     cell+ ! ; \ ( u addr -- u ) store u at index=1 of addr
: !2     [ 0x 2 CELL * ] LITERAL + ! ; \ ( u addr -- u ) store u at index=2 of addr
: !3     [ 0x 3 CELL * ] LITERAL + ! ; \ ( u addr -- u ) store u at index=3 of addr

0x 42 testCache !         testCache @ 0x 42 assertEq
0x 43 testCache !1        testCache @1 0x 43 assertEq
0x 51 testCache !2        testCache @2 0x 51 assertEq
0x 50 testCache 0x 1 !i   testCache 0x 1 @i 0x 50 assertEq
assertEmpty

\ The xt's of "!@," repsectively. It can be hard to read otherwise
: !xt ' ! ;     : @xt ' @ ;    : ,xt ' , ;

: [compile] IMM \ ( -- ) immediately compile the next word
  \ This allows you to easily compile IMM words into other words, or compile
  \ words during [ runstate ]
  word find nt>xt \ get xt for next word
  , ;             \ and compile it


\ MARK allows us to define words to be forgotten. It has many uses, but for
\ us right now the main one is we can now define tests, execute them, and
\ forget them, consuming no dictionary space.
: MARK \ consume next WORD to create dictionary snapshot. When that word is
  \ called, return the dictionary to the previous state.
  &latest @ &here @  \ get the current ( latest here )
  CREATEWORD          \ create the MARK word
  ' lit , ,         \ compile literal HERE directly
  ' &here ,   !xt , \ which the code will store into HERE
  ' lit , ,         \ Same with LATEST
  ' &latest ,   !xt , \ the word will set LATEST to previous value
  ' EXIT , ;


&here @   testCache !
&latest @ testCache !1
MARK -test
: thisWillNotExist 0x 42 ;
thisWillNotExist 0x 42 assertEq -test
\ -test     \ Neither of these lines will work if uncommented
\ thisWillNotExist
MARK -test  -test \ test mark+unmark works
testCache @   &here @ assertEq \ test: dict restored
testCache @1  &latest @ assertEq assertEmpty

\ ########################
\ # Control Structures
\ Using our BRANCH and 0BRANCH words we can create IF (ELSE?) THEN and LOOP
\ control structures. We do this by putting HERE on the stack at compile time
\ and using its value to branch backwards.

: IF IMM  \ ( -- addr )
  \ ( flag ) IF <ifblock> ELSE? <elseblock> THEN  is a conttrol structure. If flag<>0, the
  \ <ifblock> is executed. If there is an ELSE block, it will otherwise be executed.
  ' 0BRANCH , \ compile 0BRANCH, which branches to next literal if 0
  &here @     \ preserve the current location for THEN to consume
  0x 0 , ;       \ compile a dummy offset to be overriden by THEN

: THEN IMM \ ( addr -- ) Follows from IF or ELSE
  \ We want to replace the value IF jumps to to the current address.
  \ 0BRANCH uses relative jumps, so we want it to jump to here-addr
  dup &here @ swap \ ( addr here addr )
  - swap ! ;       \ Store (here-addr) at addr

: ELSE IMM \ ( addr -- addr )
  ' BRANCH , \ compile definite branch. IF was nonzero this will execute after
             \ its block
  &here @ swap \ store the current location on the stack and move to 2nd item
  0x 0 , \ put a dummy location for THEN to store to. Stack: ( elseaddr ifaddr )
  [COMPILE] THEN ; \ THEN consumes ifaddr to jump right here IF 0

MARK -test
: testIF IF 0x 42 ELSE 0x 420 THEN ;
false testIf 0x 420 assertEq    true testIf 0x 42 assertEq
0x 42 testIf 0x 42 assertEq     assertEmpty
: testIFIF IF IF 0x 42 THEN THEN ;
false testIFIF assertEmpty      false true testIFIF assertEmpty
true true testIFIF 0x 42 assertEq                   assertEmpty
-test

: BEGIN IMM \ BEGIN <block> ( flag ) UNTIL will execute <block> until flag<>0
  &here @ ; \ put the address of HERE on the stack for UNTIL to consume
: UNTIL IMM  ' 0BRANCH &HERE @ - , ; \ 0BRANCH to the offset from BEGIN
: AGAIN IMM  ' BRANCH &HERE @ - , ; \ Always branch back (infinite loop)
: BREAK IMM 

\ : WHILE IMM \ BEGIN ... ( flag ) WHILE ... REPEAT loop while flag<>0

