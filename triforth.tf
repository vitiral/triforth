\ Now that we have defined our core interpreter in triforth.S we
\ are ready to define the rest of the language.
\
\ Please note that we don't have ( comments ) until we define them,
\ so we will use line comments for everything.
&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode, nostack

\ ########################
\ # Compilation Helpers: these help us create words which compile other words.
: [compile] IMM \ ( -- ) immediately compile the next word
  \ This allows you to easily compile IMM words into other words, or compile
  \ words during [ runstate ]
  word find nt>xt \ get xt for next word
  , ;             \ and compile it

\ ########################
\ # Literals
\ We need some basic character literals to help us. We will define a few
\ words to help us as well.
: literal IMM  \ ( u -- ) take whatever is on stack and compile as a literal
  ' lit , \ compile xt of LIT
  , ;     \ compile the literal itself (from the stack)
: [ascii] IMM \ ( -- b ) compile next single-letter word as ascii literal
  ascii [compile] literal ; \ note: ascii reads from _input stream_

\ Can't be gotten with [ascii]
: '\t' 0x 9 ;     : '\n'  0x A ;    : '\r' 0x D ;
: bl   0x 20 ; \ "blank" i.e. space
\ Mess with syntax highlighters and can be hard to read
: ':' [ascii] : ;      : ';' [ascii] ; ;
: '(' [ascii] ( ;      : ')' [ascii] ) ;
: '\' [ascii] \ ;      : '"' [ascii] " ;
\ The xt's of "!@," repsectively.
: xt! ' ! ;     : xt@ ' @ ;    : xt, ' , ;

':' 0x 3A assertEq  '(' 0x 28 assertEq  ')' 0x 29 assertEq
';' 0x 3B assertEq  '\' 0x 5C assertEq  '"' 0x 22 assertEq

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
: +!     dup @ 1+ swap ! ; \ ( addr -- ) increment value inside address by 1
: +4!    dup @ 4+ swap ! ; \ ( addr -- ) increment value inside address by 4
: align \ ( addr -- addr ) align address by four bytes
  0x 3 +   [ 0x 3 invert ] LITERAL and ;
: aligned  &here @ align &here ! ; \ ( -- ) align HERE by four bytes
: b@i     + b@ ;  \ ( addr i -- b ) fetch byte at index=i of addr
: b@1     1+ b@ ;  \ ( addr -- b ) fetch byte at index=1 of addr
: b@2     0x 2 + b@ ; \ ( addr -- u ) fetch byte at index=2 of addr

0x 42 testCache !         testCache @ 0x 42 assertEq
0x 43 testCache !1        testCache @1 0x 43 assertEq
0x 51 testCache !2        testCache @2 0x 51 assertEq
0x 50 testCache 0x 1 !i   testCache 0x 1 @i 0x 50 assertEq
0x 20 testCache !   testCache +!   testcache @ 0x 21 assertEq
0x 20 testCache !   testCache +4!  testcache @ 0x 24 assertEq
0x 2 align 0x 4 assertEq     0x 4 align 0x 4 assertEq
0x 11 align 0x 14 assertEq   0x 7 align 0x 8 assertEq
assertEmpty

\ MARKER allows us to define a checkpoint to forget dictionary items. It has
\ many uses, but for us right now the main one is we can now define tests,
\ execute them, and forget them, consuming no dictionary space.
: MARKER \ consume next WORD to create dictionary snapshot. When that word is
  \ called, return the dictionary to the previous state.
  &latest @ &here @  \ get the current HERE
  CREATEWORD          \ create the word to act as a marker
  ' lit , ,         \ compile literal HERE directly
  ' &here ,   xt! , \ which the code will store into HERE
  ' lit , ,   ' &latest ,    xt! ,  \ same as HERE with LATEST
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
\ The R@N functions must NEVER be exected at runtime since
\ the return stack is corrupted by executing them.
: R@ ( -- u ) IMM ' rsp@ ,   xt@ , ;
: R@1 ( -- u ) IMM ' rsp@ ,  ' cell+ ,   xt@ , ;
\ : lroll ( u:x@N u:x@n-1 ... u:x@0 u:N -- u:x@N-1 ... u:x@0 u:x@N )

MARKER -test
assertEmpty
: testR@ 0x 42 >R   r@ 0x 42 assertEq    R> 0x 42 assertEq ;
: testR@1 0x 42 >R 0x 43 >R  r@ 0x 43 assertEq    r@1 0x 42 assertEq
  R> 0x 43 assertEq   R> 0x 42 assertEq ;
testR@ testR@1 assertEmpty -test

\ #########################
\ # Strings
\ This forth uses a unique way to specify strings that is often superior to
\ many other languages. Like many c-style languages, the `\` character "escapes"
\ the next character to be processed. For instance \n means the newline character,
\ \t the tab character, \\ is the '\' character, \0 the 0 byte, \xF2 the
\ hexidecimal byte F2, etc.  However, unlike most c-style languages, \" is the
\ start of the string and \" is also the END of the string. This means you
\ don't have to escape inner quotes. The string will ONLY end when you type
\ \". This is superior to many languages since it is extremely common
\ that you want to write  "  but rare that you need to write \"  explicitly.

: b, ( b -- ) \ append the byte onto the dictionary
  &here @ b! ( store byte to HERE) &here +! ( increment HERE) ;

MARKER -test
&HERE @ testCache !
0x 32 b,  ( add a byte, misaligning HERE ) 
testCache @ b@ ( byte at previous here) 0x 32  assertEq
&here @ 1-   testCache @ assertEq \ test: here has moved forward 1
aligned    &HERE @ 4-   testCache @ assertEq \ test: alignment moves here +4
-test

: map\" IMM ( xt -- ) \ Call an xt for each byte in a literal escaped string.
  \ The xt needs to be of this type: ( ... b -- ... )
  \ This is the core function used to define strings of various kinds in
  \ typeforth.
  \ A string can span multiple lines, but only explicit escaped newlines (\n)
  \ will insert newlines into the string. Indentation is NOT handled specially.
  \ Example:
  \ \" this string
  \     has four spaces\" 
  \ is the same as: 
  \ \" this string    has four spaces\"
  \ See the tests for more examples.
  >R BEGIN \ put xt on rstack and start loop
    \ Call KEY in a loop repeatedly, leaving TRUE for UNTIL to consume in
    \ every branch except \"
    key dup '\' = IF drop ( drop '\' ) key ( get new key )
      dup '"' = IF drop ( \" == END LOOP )  false
      ELSE dup [ascii] n = IF drop '\n' R@ execute true
      ELSE dup [ascii] t = IF drop '\t' R@ execute true
      ELSE dup '\' = IF ( '\' already on stack ) R@ execute  true
      \ TODO: \x
      \ Unknown escape, panic with error message
      ELSE >R drop _STRERROR pnt '\' emit emit '\n' emit ERR_SEE_MSG panic
      THEN THEN THEN THEN
    ELSE dup '\n' = IF drop    true \ ignore newlines
    ELSE dup '\r' = IF drop    true \ also ignore line-feeds
    ELSE R@ execute    true   \ else use byte directly
    THEN THEN THEN ( stack: count-addr count flag )
  UNTIL R> drop ;

: _litstr ( count b -- count ) \ handle bytes from map\"
  \ just compile into dict with b, and keep track of count.
  b, 1+ ;
: \" IMM 
  ' litbytes ,   &here @ ( =count-addr)  0x 0 ,
  0x 0 ( stack: count-addr count )
  ' _litstr [compile] map\"  \ map\" does the string processing
  swap !   aligned ; \ update dummy count, align HERE

MARKER -test
: strTest \" str\"          0x 3 assertEq ( count) 
  dup b@ [ascii] s  assertEq      dup b@1  [ascii] t assertEq
  b@2 [ascii] r assertEq ;                           ( run it) strTest
: quoteTest \" "q"\"        0x 3 assertEq ( count) 
  dup b@ '"'  assertEq      dup b@1  [ascii] q assertEq
  b@2 '"' assertEq ;                                ( run it) quoteTest
: escapeTest \" \\\n\t\"    0x 3 assertEq ( count) 
  dup b@ '\'  assertEq      dup b@1  '\n' assertEq
  b@2 '\t' assertEq ;                               ( run it) escapeTest
: pntTest \" **TEST \\" string\\" complete\n\"   pnt ;         pntTest assertEmpty
-test

\ #########################
\ # Character Printing helpers
\ : cr      '\n' emit ;   \ print carriage return a.k.a newline
\ : space   bl emit ;     \ print space
