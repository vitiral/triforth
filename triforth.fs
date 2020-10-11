\ Now that we have defined our core interpreter in triforth.S we
\ are ready to define the rest of the language.
\
\ Please note that we don't have ( comments ) until we define them,
\ so we will use line comments for everything.
&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode, nostack

\ ########################
\ # Literals
\ We need some basic character literals to help us. We will define a few
\ words to help us as well.
: '\n'    0x A ;
: bl      0x 20 ;       \ BL (Blank) is std forth word for space
'\n' 0x A assertEq \ test: '\n' works, nostack
: LITERAL IMM  \ ( u -- ) take whatever is on stack and compile as a literal
  ' LIT , \ compile xt of LIT
  , ;     \ compile the literal itself (from the stack)
: ':' \ ( -- c) the colon character code
  [ CHAR : ]  \ put the character ':' onto the stack at compile-time
  LITERAL ;   \ compile LIT 58 as the definition for ':' word
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

\ The xt's of "!@," repsectively. these are especially hard to read since
\ there are so many symobls.
: !xt ' ! ;     : @xt ' @ ;    : ,xt ' , ;

\ ########################
\ # Variables: allow defining variables and constants
: VARIABLE WORD create drop ; \ Allow defining variables
VARIABLE testCache 0x 0 , 0x 0 , 0x 0 , \ temp storage for tests

\ ########################
\ # Addressing and Indexing mechanisms
\ These not only allow more brevity, they are also faster since they don't use
\ multiplication or division at runtime.
: cell   0x 4 ;        \ ( -- u ) the cell size
: cell+  cell + ;  \ ( u -- u ) increment by cell size
: cells  4* ;  \ ( u -- u ) multiply by cell size
: @i     cells + @ ; \ ( addr i -- u ) fetch at index=i of addr
: @1     cell+ @ ; \ ( addr -- u ) fetch at index=1 of addr
: @2     [ 0x 2 CELLS ] LITERAL + @ ; \ ( addr -- u ) fetch at index=1 of addr
: !i     cells + ! ; \ ( u addr i -- ) store u at index=i of addr
: !1     cell+ ! ; \ ( u addr -- u ) store u at index=1 of addr
: !2     [ 0x 2 CELLS ] LITERAL + ! ; \ ( u addr -- u ) store u at index=2 of addr

0x 42 testCache !         testCache @ 0x 42 assertEq
0x 43 testCache !1        testCache @1 0x 43 assertEq
0x 51 testCache !2        testCache @2 0x 51 assertEq
0x 50 testCache 0x 1 !i   testCache 0x 1 @i 0x 50 assertEq
assertEmpty

\ MARKER allows us to define a checkpoint to forget dictionary items. It has
\ many uses, but for us right now the main one is we can now define tests,
\ execute them, and forget them, consuming no dictionary space.
: MARKER \ consume next WORD to create dictionary snapshot. When that word is
  \ called, return the dictionary to the previous state.
  &latest @ &here @  \ get the current HERE
  CREATEWORD          \ create the word to act as a marker
  ' lit , ,         \ compile literal HERE directly
  ' &here ,   !xt , \ which the code will store into HERE
  ' lit , ,         \ Same with LATEST
  ' &latest ,   !xt ,
  ' EXIT , ;        \ then the word should exit

&here @   testCache !    &latest @ testCache !1 \ store here and latest
MARKER -test
: thisWillNotExist 0x 42 ;
thisWillNotExist 0x 42 assertEq -test
\ -test     \ Neither of these lines will work if uncommented
\ thisWillNotExist
MARKER -test  -test \ test mark+unmark works
testCache @   &here @ assertEq \ test: dict restored
testCache @1  &latest @ assertEq assertEmpty

\ ########################
\ # Compilation Helpers: these help us create words which compile other words.
: [compile] IMM \ ( -- ) immediately compile the next word
  \ This allows you to easily compile IMM words into other words, or compile
  \ words during [ runstate ]
  word find nt>xt \ get xt for next word
  , ;             \ and compile it


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
: UNLESS IMM ' not ,   [compile] IF ; \ opposite of IF

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

MARKER -test
: testIF IF 0x 42 ELSE 0x 420 THEN ;
false testIf 0x 420 assertEq    true testIf 0x 42 assertEq
0x 42 testIf 0x 42 assertEq     assertEmpty
: testIFIF IF IF 0x 42 THEN THEN ;
false testIFIF assertEmpty      false true testIFIF assertEmpty
true true testIFIF 0x 42 assertEq                   assertEmpty
: testUNLESS UNLESS 0x 420 ELSE 0x 42 THEN ;
false testUNLESS 0x 420 assertEq    true testUNLESS 0x 42 assertEq
0x 42 testUNLESS 0x 42 assertEq     assertEmpty
-test

: BEGIN IMM \ BEGIN <block> ( flag ) UNTIL will execute <block> until flag<>0
  &here @ ; \ put the address of HERE on the stack for UNTIL/AGAIN to consume
: UNTIL IMM  \ ( flag -- ) BRANCH to the BEGIN offset if flag<>0
  ' NOT ,   ' 0BRANCH ,  &here @ - , ;
: AGAIN IMM  ' BRANCH , &here @ - , ; \ Always branch back (infinite loop)

MARKER -test
: testBeginUntil \ ( u:a u:b -- u:a*2^b ) for b>0 using addition only
  BEGIN
    swap dup + swap 1-  \ ( (a+a) (b-1) )
  dup UNTIL drop ;
0x 10 0x 1 testBeginUntil 0x 20 assertEq
0x 10 0x 3 testBeginUntil 0x 80 assertEq
assertEmpty -test

: WHILE IMM \ BEGIN ... ( flag ) WHILE ... REPEAT loop while flag<>0
  [compile] IF ; \ create a branch with dummy offset and push dummy addr under
: REPEAT IMM swap  \ swap so beginaddr is top, followed by whileaddr
  [compile] AGAIN  \ if this is reached, unconditional jump to BEGIN
  [compile] THEN ; \ also, modify WHILE to branch outside loop IF 0

MARKER -test
: testWhileRepeat \ same test as beginUntil
  BEGIN 1- dup 0x 0 >= WHILE
    swap dup + swap
  REPEAT drop ;
0x 10 0x 1 testWhileRepeat 0x 20 assertEq
0x 10 0x 3 testWhileRepeat 0x 80 assertEq
assertEmpty -test

\ #########################
\ # Comments - after this we can use ( block ( comments ))
: ( IMM
  0x 1 \ keep track of depth of comments
  BEGIN 
    key dup '(' = IF \ If key=open comment
      drop    1+ \ drop paren, increase depth
    ELSE ')' = IF 1- \ else if key=close comment, decrease depth
    THEN THEN
  dup UNTIL drop ; \ loop until depth=0, then drop depth and exit
( test: this is (now a ) comment )


\ #########################
\ # Mathematical functions
: /       /mod swap drop ; \ ( a b -- a/b )
: mod     /mod drop ;   \ ( a b -- a%b )
: negate  0x 0 SWAP - ; \ get negative of the number

\ #########################
\ # Stack Functions
: r@ ( -- u ) rsp@ @ ;
: r@1 ( -- u ) rsp@ cell+ @ ;
\ : lroll ( u:x@N u:x@n-1 ... u:x@0 u:N -- u:x@N-1 ... u:x@0 u:x@N )


\ #########################
\ # Character Printing helpers
: cr      '\n' emit ;   \ print carriage return a.k.a newline
: space   bl emit ;     \ print space
