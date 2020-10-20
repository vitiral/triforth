\ Now that we have defined our core interpreter in triforth.S we
\ are ready to define the rest of the language.
\
\ Please note that we don't have ( comments ) until we define them,
\ so we will use line comments for everything.
&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode, nostack

: [dbgexit] IMM dbgexit ; \ for debugging compiler state

\ ########################
\ # Compilation Helpers: these help us create words which compile other words.
\ In most languages these (and any other IMM word) would be considered macros,
\ since they essentially generate code at compile time.
: ['] IMM \ immediately get the xt of the next word and put on stack
  word find nt>xt ;
: [compile] IMM \ ( -- ) immediately compile the next word.
  \ Useful for compiling immediate words instead of executing them.
  word find nt>xt , ;
: compile, IMM \ Shorthand for  ' WORD ,
  ' lit ,    word find nt>xt ,  \ same as ' -- return xt instead of running
  ' , , ; \ and then compile it into the word YOU are compiling

\ ########################
\ # Literals
\ We need some basic character literals to help us. We will define a few
\ words to help us as well.
: literal IMM  \ ( u -- ) take whatever is on stack and compile as a literal
  compile, lit \ compile xt of LIT
  , ;     \ compile the literal itself (from the stack)
: [ascii] IMM \ ( -- b ) compile next single-letter word as ascii literal
  ascii [compile] literal ; \ note: ascii reads from _input stream_

\ Can't be gotten with [ascii]
: '\t' 0x 9 ;     : '\n'  0x A ;    : '\r' 0x D ;
: spc  0x 20 ; \ the literal ' '
\ Mess with syntax highlighters and can be hard to read
: ':' [ascii] : ;      : ';' [ascii] ; ;
: '(' [ascii] ( ;      : ')' [ascii] ) ;
: '\' [ascii] \ ;      : '"' [ascii] " ;

':' 0x 3A assertEq  '(' 0x 28 assertEq  ')' 0x 29 assertEq
';' 0x 3B assertEq  '\' 0x 5C assertEq  '"' 0x 22 assertEq \ "

\ ########################
\ # Variables: allow defining variables and constants
: VARIABLE WORD create drop ; \ Allow defining variables
VARIABLE &testCache 0 , 0 , 0 , \ temp storage for tests

\ ########################
\ # Addressing and Indexing mechanisms
\ These not only allow more brevity, they are also faster since they don't use
\ multiplication or division at runtime.
: cell IMM   compile, 4 ;        \ ( -- u ) the cell size
: cell+  cell + ;  \ ( u -- u ) increment by cell size
: cell-  cell - ;  \ ( u -- u ) decrement by cell size
: cells  4* ;  \ ( u -- u ) multiply by cell size
: @i     cells + @ ; \ ( addr i -- u ) fetch at index=i of addr
: @1     cell+ @ ; \ ( addr -- u ) fetch at index=1 of addr
: @2     [ 2 CELLS ] LITERAL + @ ; \ ( addr -- u ) fetch at index=1 of addr
: !i     cells + ! ; \ ( u addr i -- ) store u at index=i of addr
: !1     cell+ ! ; \ ( u addr -- u ) store u at index=1 of addr
: !2     [ 2 CELLS ] LITERAL + ! ; \ ( u addr -- u ) store u at index=2 of addr
: +1!     dup @ 1+ swap ! ; \ ( addr -- ) increment value inside address by 1
: +4!    dup @ 4+ swap ! ; \ ( addr -- ) increment value inside address by 4
: align \ ( addr -- addr ) align address by four bytes
  0x 3 +   [ 0x 3 invert ] LITERAL and ;
: aligned  &here @ align &here ! ; \ ( -- ) align HERE by four bytes
: b@i     + b@ ;  \ ( addr i -- b ) fetch byte at index=i of addr
: b@1     1+ b@ ;  \ ( addr -- b ) fetch byte at index=1 of addr
: b@2     2 + b@ ; \ ( addr -- u ) fetch byte at index=2 of addr

0x 42 &testCache !         &testCache @ 0x 42 assertEq
0x 43 &testCache !1        &testCache @1 0x 43 assertEq
0x 51 &testCache !2        &testCache @2 0x 51 assertEq
0x 50 &testCache 1 !i      &testCache 1 @i 0x 50 assertEq
0x 20 &testCache !   &testCache +1!   &testCache @ 0x 21 assertEq
0x 20 &testCache !   &testCache +4!  &testCache @ 0x 24 assertEq
2 align 4 assertEq     4 align 4 assertEq
0x 11 align 0x 14 assertEq   0x 7 align 8 assertEq
assertEmpty

\ MARKER allows us to define a checkpoint to forget dictionary items. It has
\ many uses, but for us right now the main one is we can now define tests,
\ execute them, and forget them, consuming no dictionary space.
: MARKER \ consume next WORD to create dictionary snapshot. When that word is
  \ called, return the dictionary to the previous state.
  &latest @ &here @  \ get the current HERE
  CREATEWORD          \ create the word to act as a marker
  compile, lit ,         \ compile literal HERE directly
  compile, &here   compile, ! \ which the code will store into HERE
  compile, lit ,   ' &latest ,    compile, !  \ same as HERE with LATEST
  compile, EXIT ;        \ then the word should exit

&here @   &testCache !    &latest @ &testCache !1 \ store here and latest
MARKER -test
: thisWillNotExist 0x 42 ;
thisWillNotExist 0x 42 assertEq -test
\ -test     \ Neither of these lines will work if uncommented
\ thisWillNotExist
MARKER -test  -test \ test mark+unmark works
&testCache @   &here @ assertEq \ test: dict restored
&testCache @1  &latest @ assertEq assertEmpty

\ ########################
\ # Control Structures
\ Using our BRANCH and 0BRANCH words we can create IF (ELSE?) THEN and LOOP
\ control structures. We do this by putting HERE on the stack at compile time
\ and using its value to branch backwards.

\ ####
\ # IF ( ELSE ) THEN
: IF IMM  \ ( -- )
  \ ( flag ) IF <ifblock> ELSE? <elseblock> THEN  is a conttrol structure. If flag<>0, the
  \ <ifblock> is executed. If there is an ELSE block, it will otherwise be executed.
  compile, 0BRANCH \ compile 0BRANCH, which branches to next literal if 0
  &here @     \ preserve the current location for THEN to consume
  0x 0 , ;       \ compile a tmp offset to be overriden by THEN
: THEN IMM \ ( addr -- ) Follows from IF or ELSE
  \ We want to replace the value IF jumps to to the current address.
  \ 0BRANCH uses relative jumps, so we want it to jump to here-addr
  dup &here @ swap \ ( addr here addr )
  - swap ! ;       \ Store (here-addr) at addr
: ELSE IMM \ ( addr -- addr )
  compile, BRANCH \ compile definite branch. IF was nonzero this will execute after
             \ its block
  &here @ swap \ store the current location on the stack and move to 2nd item
  0x 0 , \ put a tmp location for THEN to store to. Stack: ( elseaddr ifaddr )
  [COMPILE] THEN ; \ THEN consumes ifaddr to jump right here IF 0

MARKER -test
: testIF IF 0x 42 ELSE 0x 420 THEN ;
false testIf 0x 420 assertEq    true testIf 0x 42 assertEq
0x 42 testIf 0x 42 assertEq     assertEmpty
: testIFIF IF IF 0x 42 THEN THEN ;
false testIFIF assertEmpty      false true testIFIF assertEmpty
true true testIFIF 0x 42 assertEq                   assertEmpty
-test

\ ####
\ # BEGIN UNTIL  |  BEGIN AGAIN
: BEGIN IMM \ BEGIN <block> ( flag ) UNTIL will execute <block> until flag<>0
  &here @ ; \ put the address of HERE on the stack for UNTIL/AGAIN to consume
: UNTIL IMM  \ ( flag -- ) BRANCH to the BEGIN offset if flag<>0
  compile, NOT   compile, 0BRANCH   &here @ - , ;
: AGAIN IMM  compile, BRANCH  &here @ - , ; \ Always branch back (infinite loop)

MARKER -test
: testBeginUntil \ ( u:a u:b -- u:a*2^b ) for b>0 using addition only
  BEGIN
    swap dup + swap 1-  \ ( (a+a) (b-1) )
  dup UNTIL drop ;
0x 10 1    testBeginUntil 0x 20 assertEq
0x 10 0x 3 testBeginUntil 0x 80 assertEq
: testBeginAgain \ same as above
  BEGIN
    swap dup + swap 1-        \ ( (a+a) (b-1) )
    dup =0 IF drop EXIT  THEN \ if b=0 exit
  AGAIN ;
0x 10 1    testBeginAgain 0x 20 assertEq
0x 10 0x 3 testBeginAgain 0x 80 assertEq
assertEmpty -test

\ ####
\ # BEGIN WHILE REPEAT
: WHILE IMM \ BEGIN ... ( flag ) WHILE ... REPEAT loop while flag<>0
  [compile] IF ; \ create a branch with tmp offset and push tmp addr under
: REPEAT IMM swap  \ swap so beginaddr is top, followed by whileaddr
  [compile] AGAIN  \ if this is reached, unconditional jump to BEGIN
  [compile] THEN ; \ also, modify WHILE to branch outside loop IF 0

MARKER -test
: testWhileRepeat \ same test as beginUntil
  BEGIN 1- dup 0x 0 >= WHILE
    swap dup + swap
  REPEAT drop ;
0x 10 1 testWhileRepeat 0x 20 assertEq
0x 10 0x 3 testWhileRepeat 0x 80 assertEq
assertEmpty -test

\ #########################
\ # Comments - after this we can use ( block ( comments ))
: ( IMM
  1 \ keep track of depth of comments
  BEGIN 
    key dup '(' = IF \ If key=open comment
      drop    1+ \ drop paren, increase depth
    ELSE ')' = IF 1- \ else if key=close comment, decrease depth
    THEN THEN
  dup UNTIL drop ; \ loop until depth=0, then drop depth and exit
( test: this is (now a ) comment )

\ #########################
\ # Additional Mathematical Functions
: /       /mod swap drop ; \ ( a b -- a/b )
: mod     /mod drop ;   \ ( a b -- a%b )
: negate  0 SWAP - ; \ get negative of the number
: u8 ( u -- u8 ) 0x FF AND ; \ mask upper bits
: land ( flag flag -- flag \logical and)
  IF IF true ELSE false THEN
     ELSE drop false THEN ;
: lor ( flag flag -- flag \logical or)
  IF drop true ELSE IF true ELSE false THEN THEN ;

\ #########################
\ # Stack Operations
: Rdrop ( -- \ drop cell on R) IMM  compile, R>  compile, drop ;
: R@ ( -- u ) rsp@ cell + @ ; \ add cell to skip caller's address
: R@1 ( -- u ) rsp@ 2 cells + @ ; \ 2 cells because we have to skip caller's address
: R@2 ( -- u ) rsp@ 0x 3 cells + @ ;
: -R@ ( -- \increment R@ ) rsp@ cell + +! ;
: -R@ ( -- \decrement R@ ) rsp@ cell + -! ;
: 2>R ( u:a u:b -- R: a b ) IMM compile, 2 compile, n>R ; \ R: ( -- a b)
: 2R> ( -- u:a u:b ) IMM        compile, 2 compile, nR> ; \ R ( a b -- )
: 2Rdrop ( -- \ drop 2cells on R) IMM  [compile] 2R>  compile, 2drop ;
: >R@ ( u -- u \ store+fetch) IMM compile, dup  compile, >R ; \ same as >R R@
: 2>R@ ( u64 -- u64 \ store+fetch) IMM compile, 2dup  compile, 2>R ;
: swapAB ( ... A B -- ... ) \ swap size A with size B
  \ TODO: check that memory is large enough and no stack overflow
  \ Naming: B is the size, &B is a pointer to the data. Same with A
  \ So, we want to move &A -> &B and &B -> &A. We use temporary storage on the stack
  >R@ ( =B) DSP@ 2 cells + >R@ ( =&B)
  dup over 0x 16 + - >R@ ( =tmp dest) lrot cellmove \ move B to tmp stack
  \ D: ( A)  R: ( B &B &tmp) -- we now want to move &A down to &B
  R@1 ( =&B) R@2 ( =B) cells + ( =&A src) R@1 ( =&B dst) lrot >R@ ( =A) cellmove
  \ D: ( )   R: ( B &B &tmp A) we need to move B to where A was
  R> ( =A) R@1 ( =&B) + ( =dst &A)  R> ( =&tmp src) swap Rdrop R> ( =B )
  cellmove ;


MARKER -test
assertEmpty
: testR@ 0x 42 >R   r@ 0x 42 assertEq    R> 0x 42 assertEq ; testR@
: testR@1 0x 42 >R 0x 43 >R  r@ 0x 43 assertEq    r@1 0x 42 assertEq
  R> 0x 43 assertEq   R> 0x 42 assertEq assertEmpty ; testR@1
: test2>R 0x 42 0  2 n>R   R@ 0 assertEq  R@1 0x 42 assertEq
          2R>  0 asserteq   0x 42 assertEq assertEmpty ; test2>R
: testR@2 0x 42 >R 0 >R 0 >R assertEmpty R@2 0x 42 assertEq 2Rdrop Rdrop
  ; testR@2
: test-R@ 0x 42 >R -R@ 0x 41 R> assertEq ; test-R@
: testRSP@ \ test some assumptions about RSP
  0x 2 >R RSP@ @ 0x 2 assertEq   0x 3 RSP@ ! ( store 3)
  RSP@ @ 0x 3 assertEq R> 0x 3 assertEq ; testRSP@
\ TODO: testSwapAB
-test

\ #########################
\ # Strings and Formatting
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
  &here @ b! ( store byte to HERE) &here +1! ( increment HERE) ;

MARKER -test
&HERE @ &testCache !
0x 32 b,  ( add a byte, misaligning HERE ) 
&testCache @ b@ ( byte at previous here) 0x 32  assertEq
&here @ 1-   &testCache @ assertEq \ test: here has moved forward 1
aligned    &HERE @ 4-   &testCache @ assertEq \ test: alignment moves here +4
-test

: map\" ( xt -- ) \ Call an xt for each byte in a literal escaped string.
  \ The xt needs to be of this type: ( ... u8 bool:escaped -- ... )
  \ where "escaped" lets the xt know if the character was escaped or not.
  \ 
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
    \ Call KEY in a loop repeatedly until \" -- passing each character onto xt
    key dup '\' = IF drop key ( drop '\' and get new)
      dup '"' = IF drop false ( =end loop)
      ELSE true ( =escaped) R@ execute  true
      THEN
    ELSE false ( =escaped) R@ execute   true
    THEN ( stack: count-addr count flag )
  UNTIL R> drop ;

: _litsStart ( -- addr-count count ) \ Create tmp string literal
  ' litcarru8 ,   &here @ ( =count-addr) 0 b, ( =tmp-count) 0 ( =count) ;
: _litsFinish ( addr-count count ) \ update tmp count, align HERE
  \ TODO: support count [256,u16MAX] with litc2arru8
  dup 0x 255 > IF _SCERR .Sln panic THEN 
  swap b! ( update tmpcount)  aligned ;
: _lits ( count b bool:escape -- count ) \ handle bytes from map\"
  \ Compiles relevant characters, checks escape validity.
  swap \ check for ignored characters
  dup '\n' = IF 2drop EXIT THEN    \ ignore newlines and "\\\n"
  dup '\r' = IF 2drop EXIT THEN    \ ignore line-feeds and "\\\r"
  swap IF \ escaped character, convert to known character or panic
      dup [ascii] n =       IF drop '\n'
      ELSE dup [ascii] t =  IF drop '\t'
      ELSE dup '\' =        IF ( '\' literal)
      ELSE ( unknown escape) _SERR .s '\' emit emit '\n' emit panic
      THEN THEN THEN
  THEN   b, ( <-compile byte) 1+ ( <-inc count) ;
: \" IMM    _litsStart   ' _lits map\"    _litsFinish ; 

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
: pntTest \" **TEST \\" string\\" complete\n\"   .s ;        pntTest assertEmpty
-test

: .ln     '\n' emit ;   \ print new line
: .spc    spc emit ;    \ print space

\ exec\" function allows executing words in the context of strings. Each string
\ will be executed given the provided xt and any included "$NAME " will be executed
\ in this context. This allows for arbitrary behavior like printing to stdout or
\ writing to a file, etc.
\ Note: the space (or other whitespace) at the end of "$NAME " is consumed and
\ is necessary. When this function encounters a $ it
\ simply does  WORD FIND NT>XT EXEC|COMPILE to get the xt and pretend it is the interpreter. 
\ WORD consumes the extra space.
\ Note: $ can be escaped with \$

: _exec \ ( 'str addr-count count u8 unknown-escaped -- 'str addr-count count )
  \ 'str must be xt of type ( addr count -- )
  IF ( handle unknown escape) 
    dup [ascii] $ = IF false _lits \ if \$ pass as unescaped $
    ELSE true _lits \ else pass to _lits as escaped which handles or panics
    THEN EXIT
  THEN
  dup [ascii] $ = IF
    \ - Finsh the previous litstr
    \ - Compile the xt to run on litstrings into the word
    \ - exec|compile the xt specified in the string
    \ - start a new litstr
    drop ( drop key) _litsFinish    dup ( ='str) exec|compile
    word ( word from STRING) find nt>xt exec|compile   _litsStart ( 'str addr-count count)
  ELSE false _lits THEN ;

: exec\" IMM ( 'str -- )
  _litsStart   ' _exec    map\"  _litsFinish   ( 'str) ,  ;
: .f\" IMM ( -- ) \ Emits the formatted string to emitFd
  ' .s  [compile] exec\" ;
: .fln\" IMM ( -- ) \ Emits the formatted string followed by newline
  ' .s  [compile] exec\"   compile, .ln ;

: } .fln\" only use } with {\"  panic ;
: { IMM \ compile-block. This calls exec|compile on all words until }.
  \ At first this may seem useless since it is what the interpreter already
  \ does, but when you combine it with strings or ? (below) it allows you to
  \ execute any arbitrary code instead of only a single statement.
  \ ( Ex) \" foo${ any code here } bar\"
  \ ( Ex) doSomethingDangerous ? { error handling code } ( ... rest of function)
  ' [compile] } ( <xt of }> ) BEGIN
    word
    find nt>xt 2dup = IF 2drop EXIT
    ELSE  exec|compile
    THEN
  AGAIN ;

MARKER -test
: testExec \" World\"   ['] .s exec\" Hello $.s !\n\"  ; testExec
: test.f \" Denver\"  .f\" Hello $.s !\n\" ; test.f
: test.fln \" Arvada\"  .f\" Hello $.s !\n\" ; test.fln
-test

\ #########################
\ # Testing Harness and Debugging Tools
\ Okay, now that we have awesome string formatting, we can build out a better
\ test harness and debugging tools.
\ dumpInfo: we will replace the old one with one that also prints the return stack
\   along with the name of the word (or ??? if it doesn't know).

: between ( min max i -- flag ) \ return whether i is bettween [min,max)
  dup >= IF false THEN   < IF false THEN true ;
: checkRead ( addr -- flag ) \ false if addr is not within valid memory
  RSMIN HEAPMAX lrot between ;
: XT>&F0 4- 4- ;
: XT>NT  XT>&F0 dup ( &F0 contains namelen, so find name to left)
  &F0>NAMELEN 1+ align ( =aligned name+countbyte)
  - ( =&name) 4- ( =nt) ;
: findMap ( xt -- nt:found ) \ call xt on every nt in the dictionary until
  \ EXECUTE returns true. Return that nt or 0 if none found.
  >R ( R@1=xt)  &latest @ >R ( R@=nt)
  BEGIN
    R@ =0 IF ( nt=0, dict exhausted) 2Rdrop 0 EXIT THEN
    ( nt xt EXECUTE) R@ R@1 execute
    IF ( found) R> ( =nt) Rdrop ( drop xt) EXIT THEN
    R> @ ( go to next item) >R
  AGAIN ;
: _xe  ( xt:a nt -- xt:a flag ) \ return whether a is the xt of nt
  over swap ( xt xt nt ) nt>xt = ;
: xtExists ( xt -- flag ) \ return whether the xt exists.
  ' _xe findMap nip ;

MARKER -test
: _fakeFind  ( addr count nt -- addr count flag)
  >R ( R@=nt) 2dup ( dup str) R> nt>str bytesEqNoCase ;
: testXT>NT   ' swap ( =xt of swap) XT>NT \" swap\" find assertEq 
  ' .spc XT>NT \" .spc\" find assertEq ; testXT>NT
: testFindMap  
  \" swap\" ' _fakeFind findMap   ' swap XT>NT assertEq 2drop ( drop str) 
  \" dne\"  ' _fakeFind findMap              0 assertEq 2drop ( drop str) 
  ; testFindMap
: testXtExists 0x 88888888 xtExists assertFalse   ' swap xtExists assertTrue
  ; testXtExists
-test

: R@..0 ( u -- ) \ repeat the next block R@ times, decrementing R@ each time.
  [compile] BEGIN compile, R@  [compile] WHILE
    word find nt>xt exec|compile  compile, compile, -R@
  [compile] REPEAT compile, Rdrop ;

: uBASE>ascii ( u base -- quot ascii ) 
  \ convert integer to the remaining integer and ascii character.
  /mod swap ( quot remainder ) u>ascii ;
: uBASE>str ( u base -- addr count )
  \ Converts the unsigned integer with the base to ascii. Note:
  \ This uses space 6 cells above the return stack for temp storage.
  \ The number should be used or moved quickly.
  >R ( R@2=base)
  dup =0 IF u>ascii emit EXIT THEN
  RSP@ 0x 6 cells - 1- >R ( R@1=&tmp ) 0 >R ( R@=index)
  \ Note: we write DOWN because we read least-significant digits first
  BEGIN ( dstack: u ) R@2 ( =base) uBASE>ascii
     R@1 R@ - ( =&tmp-index) b!   RSP@ +! ( inc index)
  dup UNTIL drop R@1 R@ - 1+ ( &tmp-index+1) R> ( addr index) 2Rdrop ;
: .ub ( u -- ) 0x 2 uBASE>str .s ; \ print as binary

: "???" \" ???\" ;
: _n? ( &code nt -- &code flag:nt<=&code )  over <= ;
: &code>name? ( &code -- addr count )
  dup &here @ >= IF drop "???" EXIT THEN
  ' _n? findMap nip dup IF nt>name ELSE drop "???" THEN ;
: .rstack ( -- ) \ print the return stack, trying to find the names of the words
  \ Since data as well as code addresses are on the return stack, we won't
  \ be completely accurate, but it will probably help when panicing.
  \ Also, we print the values of the data, which will be helpful when debugging
  \ values on the return stack.
  RSMAX RSP@ - 4/ ( =rstack depth) .f\" RSTACK <$.ux >:\n\"
  RSMAX cell - BEGIN dup RSP@ cell - <> WHILE \ go through return stack
    .f\"   ${ dup @ .ux }  :: ${ dup @ &code>name? .s } \n\"
    cell - \ next Rstack cell
  REPEAT drop ;
: panicHandler ( -- ) .rstack asmpanic ;
&latest @ nt>xt &'PANIC ! \ set panic to new handler
: .!! .f\" \n!!!!!!!!!!!\n!! PANIC: \" ;
: panicf\" IMM  compile, .!!  [compile] .fln\"  compile, panic ;
\ : doPanic 2 panicf\" This $.ux  Rocks!\" ; doPanic  \ uncomment to see in action

: runAsTest
  &latest @ nt>name  &wordLen ! wordBuf &wordLen @ bmove
  &latest @ nt>xt execute assertEmpty
  TEST_PREFIX_NT countnt .s   &latest @ nt>name .s   TEST_PASS_NT countnt .s ;

\ #########################
\ # Errors and Error Hanlding
\ In this forth there are two kinds of errors:
\ - panic: we have already seen this. This aborts the program immediately,
\   printing out some debug information. This is the correct error in cases
\   where the _programmer_ made a mistake, i.e. by using too many stack values.
\   Panics should NOT be caught (outside of tests), as there is no way to
\   clean up any leftover state.
\ - Result enum type. This has two possible values: Ok and Error and uses the
\   ? operator (which uses ?0BRANCH) to handle errors cleanly.

: IF? IMM  \ ( flagu8 -- flagu8 ) Check IF least-significant-byte of stack is 0.
  compile, ?0BRANCH  &here @  0 , ;   \ see IF for explanation
: ? IMM \ If error, execute next statement and exit.
  \ "error" is defind by any non-zero value in the least-significant-byte
  \ of the top value on the stack. This allows other values to be stored
  \ in the upper 3 bytes. This function uses 0u8branch, so does not affect
  \ the stack. Note: these are explicitly defined to work with valued enums,
  \ which we will define later.
  \ 
  \ Conceptually, checks the byte in top value on the stack, if it is non-zero
  \ executes the next word and exits with the error. Example:
  \   >R@ doSomethingDangerous ? Rdrop ( ... rest of word)
  \ If doSomethingDangerous returns a non-zero value then Rdrop will be called.
  [compile] IF?
    word find nt>xt exec|compile    compile, EXIT
  [compile] THEN ;
: _ IMM  ; \ Noop, useful with ? if there is no cleanup.

MARKER -test
: testIF?  0    IF? failTest                THEN 0 assertEq
  false         IF? failTest                THEN false assertEq
  0x 7FFFFFFF   IF? failTest                THEN 0x 7FFFFFFF assertEq
  F_ERR         IF? F_ERR assertEq          ELSE failTest THEN
  true          IF? true assertEq           ELSE failTest THEN
  F_ERR 0x FF01 OR IF? 0x 8000FF01 assertEq           ELSE failTest THEN
  ;  runAsTest
: test?  0     ? failTest              0 assertEq
  false        ? failTest              false assertEq
  0x FF00      ? failTest              0x FF00 assertEq
  F_ERR        ? { drop }              failTest ; RUNASTEST
: test?2 true         ? { true assertEq }     failTest ; RUNASTEST
: test?3 0x FF010000  ? { 0x FF010000 assertEq }  failTest ; RUNASTEST
-test

\ The Result enum which has two possible values: Ok<V> or Error<E>.  The F_ERR
\ flag takes up a single bit, the highest one, leaving 31 bits to be used for
\ any kind of data (V or E).
: Error ( u -- Error ) F_ERR OR ;  \ sets the F_ERR bit
: Ok    ( u -- Ok )    F_ERR INVERT AND ; \ clears the F_ERR bit

\ Similar to Result there is the Option enum which has two values:
\   None: 0 means there is no value
\   Some: any other value is Some, the value is obtained by subtracting 1
\ You "create" an option with None (which is just 0) or  v Some which
\ just adds 1 to v. You then "get" the value using IFsome which will
\ automatically subtract 1 inside the "IF" block and otherwise drop the value.
\ 
\ One negative (pun intended): negative values are more difficult to store,
\ because  -1 Some == None. Therefore Option is not a good container for negative
\ values (but if you really needed them, create a NegOption type)
: None IMM  compile, 0 ;
: Some IMM  compile, 1+ ;
: IFsome IMM  compile, NONEBRANCH  &here @  0 , ; \ see IF for explanation

MARKER -test
: testNone  None IFsome failTest THEN ; RUNASTEST
: testSome  1 Some IFsome 1 assertEq ELSE failTest THEN  ; RUNASTEST
: testSome2  0x 100 Some IFsome 0x 100 assertEq ELSE failTest THEN  ; RUNASTEST
: testSome-1  1 negate Some IFsome failTest THEN ; RUNASTEST \ demonstrate odd behavior
: testSomeOk 0x 42 Ok Some  ( Some<Ok<0x42>>) 
  IFsome ? failTest  0x 42 assertEq ELSE failTest THEN ; RUNASTEST
: testSomeErr 0x 666 Error Some  ( Some<Error<0x666>>)
  IFsome ? { 0x 666 Error assertEq } failTest ELSE failTest THEN ; RUNASTEST
-test

: sysexit ( rc -- ) SYS_EXIT SYSCALL1 ;
: syswrite ( count addr fd -- eax )
  \ Calls syswrite which returns the count written or a negative errno
  SYS_WRITE SYSCALL3 ;
: writefd ( addr count fd -- u24Result )
  rrot swap lrot ( count addr fd )
  3dup syswrite dup 
  0 < IF negate F_ERR THEN ; \ if <0 set F_ERR flag

\ TODO: writefdall would be nice

\ #########################
\ # Memory Manager
\ Like most things in this forth, our memory manager is going to be _extremely_
\ simple. However it should actually be reasonably performant and resistent to
\ fragmentation. It will have the disandvantage that you can only allocate up
\ to 1k blocks (0m1024 i.e. 0x400 bytes) of memory. Data structures (besides
\ the dictionary itself) we create will have to be designed around this
\ constraint.
\ 
\ We are using an **arena** alocator. Arena allocators allow you to
\ create an Arena and allocate and free memory in powers of 2 from it. We will
\ also have a basic defrag strategy. The advantage of arenas however is that
\ you can drop the _entire_ arena at once, which guarantees no fragmentation.
\ We will call our arena allocator "a1k" meaning "arena 1 killobyte"
\ 
\ The primary data type of our arena will be singly-linked lists. Free blocks
\ will be kept as linked lists. When memory is freed the caller calls
\   : a1k.free ( ptr po2 &arena -- ) ... ;
\ where po2 is the "power of 2" size of ref This will convert the first cell to
\ a pointer to the next free memory, meaning we only have to keep pointers to
\ the first free memory we find.
\ 
\ Allocation happens via the function
\   : a1k.alloc ( po2 &arena -- Option<ptr> ) ... ;
\ 
\ Allowed powers of 2 start at 4 bytes (2^2) and end at 1024 bytes (2^A),
\ so we need A - 2 = 8 pointers to our free linked lists.
\ 
\ There will be two "types" of arenas -- the root arena which is allowed
\ to hold references to 1k blocks and non-root arenas which are not.
\ Non-root arenas have a reference to the root arena so they can request
\ 1k blocks.

\ a1k is a 64 byte data structure with the following format:
\ STRUCT a1k
\   free: [&sll; 8] \ 8 pointers to singly linked lists containing free po2 mem
\   root: &root     \ pointer to a root, or 0 if this is root
\   &sll<arena,&1kmem> \ (root only) sll with which arena is allocating 1k blocks
\ END

\ Singly linked list. These are insanely simple. You start with a "root" address
\ that contains 0 (null) and use it's address as &next for insert. Pop
\ works in reverse.

\ root -> [ next, ...] -> ... -> [ 0, ... ] \ note: root and next are addresses
: sll.insert ( &prev &self -- \insert item after prev)
  \ .fln\"   ?? sll.insert: $.stack &prev@=${ over @ .ux }  &self@=${ dup @ .ux } \"
  >R ( R@=&self) dup @ ( =&prev.&next) R@ ! ( <-store at &self)
  R> swap ! ( <-store &self in next of prev) ;
: sll.poproot ( &root -- &sll )
  dup @ =0 IF drop 0 ( empty) EXIT THEN
  dup @ >R@ ( R@=&sll to return) @ ( =&sll.next) swap ! ( set as root)
  R> ;

MARKER -test                        ( first)  ( second)
VARIABLE &root 0 ,  VARIABLE &values 1 , 2 ,    0x 3 , 4 ,
: testInsert &root &values sll.insert
  &root @ &values assertEq  &values @ 0 assertEq 
  &root &values 2 cells + sll.insert \ insert next 
  &root @ &values 2 cells + assertEq \ root -> second
    &values 2 @i &values assertEq    \ second -> first
    &values @ 0 assertEq ; RUNASTEST \ first -> null
: testPop 0 &root !   &root sll.poproot  0 assertEq
  &root &values sll.insert   &root &values 2 cells + sll.insert
  &root sll.poproot dup &values 2 cells + ( ==second) assertEq
    @ &values ( second.next==first) assertEq
    &root @ &values ( root now == first) assertEq 
  &root sll.poproot dup &values ( ==first) assertEq
    @ 0 ( first.next=0) assertEq
    &root @ 0 ( root=0) assertEq ; RUNASTEST
-test

: memsplit ( po2 &mem -- &first &second ) \ split memory into two po2 sized chunks
  dup rrot ( &mem po2 &mem) swap 1
  ( &mem &mem 1 po2) Nshl  ( &mem &mem 1*2^po2) + ;
: cellerase ( addr count -- \zero count cells at address)
  BEGIN 1- 2dup cells + 0 swap ! dup UNTIL 2drop ;

MARKER -test
: testZero  4 &testCache !  2 &testCache 2 cells + !
  &testCache 2 cellerase    &testCache @ 0 assertEq
  &testCache @1 0 assertEq
  &testCache 2 cells + @ 2 assertEq ; RUNASTEST

: a1k.&root ( &self -- &root ) 0x 9 cells + ;
: a1k.po2Max 0x A ; \ 2^10=x400= 1kB (decimal) bytes
: a1k.po2Min 2 ; \ 2^2=4 bytes
: a1k.po2Chk ( po2 -- po2 ) \ check the po2 is valid.
  dup a1k.po2Min a1k.po2Max 1+ lrot between
  NOT IF panicf\" po2 invalid: $.ux \" THEN  ;
: a1k.isPo2Max ( po2 -- flag ) 0x A = ; \ only root provides max Po2 blocks
: a1k.Po2i ( po2 -- po2i ) \ get the Po2 index ( byte increment from &self)
  2- ( 2^0 and 2^1 not supported) cells ;
: a1k.Po2Sll&Root ( po2 &self -- &root \return &root for sll of Po2) swap a1k.Po2i + ;
: a1k.allocMax ( &self -- ptr ) \ only root can hold 1k blocks
  dup a1k.&root @ dup IF ( not root) nip a1k.allocMax EXIT THEN
  ( is root, drop null &root) drop a1k.po2Max swap ( po2Max &self) 
  a1k.Po2Sll&Root sll.poproot ;
: a1k.freeMax ( &mem &self -- ) \ only root can hold 1k blocks
  dup a1k.&root @ dup IF ( not root) nip a1k.freeMax EXIT THEN
  drop a1k.po2Max swap ( &mem po2Max &self) a1k.Po2Sll&Root swap sll.insert ;
: a1k.freeNoMerge ( &mem po2 &self -- ) \ mark a block as freed and store in ll
  swap a1k.po2Chk dup a1k.isPo2Max IF drop a1k.freeMax EXIT THEN
  a1k.Po2Sll&Root swap sll.insert   swap ;
: a1k.free ( &mem po2 &self -- ) \ mark a block as freed and attempt to merge up
  3drop \ TODO
  ; 
: a1k.alloc ( po2 &self -- ptr )
  swap a1k.po2Chk dup a1k.isPo2Max IF drop a1k.allocMax EXIT THEN
  ( attempt to find free Po2, or create a new one) >R ( R@1=po2)  >R ( R@=&self)
  R@1 R@ a1k.Po2Sll&Root sll.poproot ( maybe free block)
  dup IF ( found free block) 2Rdrop EXIT THEN
  \ Otherwise, we need to ask for memory from the next-size up, and it may have to do
  \ the same. This is a classic solution for recursion, but it might recurse 8x, which
  \ is a high memory cost. Instead, we are going to walk up untill we find memory.
  \ Then walk down, splitting the memory chunks in half.
  R@1 ( =po2inc) BEGIN 1+ ( po2inc+=1)
    dup a1k.isPo2Max IF ( 1k block)
      a1k.allocMax dup IF false ( po2=1k &mem1k false \ end loop)
      ELSE nip 2Rdrop ( could not get 1k block, exit with 0) EXIT THEN
    ELSE dup ( =po2inc) R@ a1k.Po2Sll&Root sll.poproot ( po2 sll-found?)
      dup IF false ( stop loop) ELSE drop true ( continue loop) THEN
    THEN
  UNTIL
  \ Now we have a block of memory that is po2inc in size, but we need R@1=po2 size.
  \ Simply split memory, storing free blocks, until we have the right size.
  ( po2inc &mem_po2inc) BEGIN swap 1- dup ( &mem_po2dec+1 po2dec po2dec) 
    lrot memsplit ( po2dec &mem_po2dec &mem_po2dec) 2 pick swap a1k.freeNoMerge
    ( po2dec &mem_po2dec) over R@1 = IF ( we've found the memory we need) nip EXIT THEN
    ( else loop again, splitting the &mem again)
  AGAIN ;
: a1k.init ( &root &self) 
  >R@ ( R@=&self)   a1k.po2Max a1k.po2Min -  cellerase ( <-zero sll roots)
  R> a1k.&root ! ( <- store &root) ;
: a1k.initroot ( &memstart num1kBlocks &self -- )
  .fln\" ?? a1k.initroot: $.stack \"
  dup 0 swap a1k.init ( <- init with &root=0)
  a1k.po2Max swap a1k.Po2Sll&Root
  0x 3 n>R ( R@2=&memstart R@1=num1kBlocks R@=&1k-sll-root)
  0 BEGIN dup R@1 < WHILE
    dup a1k.po2Max Nshl ( =i*2^po2Max) R@2 + ( =&mem) R@ swap
    .fln\" ?? INSERTING: $.stack &mem@=${ over @ .ux }  &1k-sll-root@=${ R@ @ .ux } \"
    sll.insert
    \ .fln\"  ?? after:&1k-sll-root@=${ R@ @ .ux } \"
  1+ REPEAT 0x 3 nR> 4drop ;

\ Initialize &&a1kroot with 0x40 KiB (0m64KiB) of memory taken from the heap.
VARIABLE &heap
  HEAPMAX  0x 400 0x 6 Nshl ( 0m1KiB * 2^6 = 0x40 KiB = 0m64KiB) - ,
( test) 0x 400 0x 6 Nshl 0x 10000 assertEq
( test) &heap @ 0x 10000 + HEAPMAX assertEq

&heap @ 0x 40 - &heap ! ( allocate 0x40 bytes on heap)
VARIABLE &&a1kroot  &heap @ ,
MARKER -pnt 
: pnt .fln\" a1kroot=${ &&a1kroot @ .ux }  &mem0=${ &&a1kroot @ 0x 40 + .ux }
  heapmax=${ heapmax .ux } \"
; pnt -pnt

\ initialize root allocator.
&&a1kroot @ 0x 40 + ( =&mem) 0x 40 ( =num1kBlocks) &&a1kroot @ a1k.initroot

MARKER -test
VARIABLE &mem0   &heap @ 0x 40 + , ( =start of mem, 1k-block0)
VARIABLE &mem39  HEAPMAX 0x 400 - , ( last block 0x400 before heapmax)
VARIABLE a1kroot.&sll1k   &&a1kroot @ 8 cells + ,

: testA1k.init
  .fln\" ?? a1kroot.&sll1k ${ a1kroot.&sll1k @ .ux } \"
  .fln\" ??? DUMP &&a1kroot ${ &&a1kroot @ 0x 40 dump } \"
  ( &&a1kroot is at the &heap) &&a1kroot @ &heap @ assertEq
  ( &mem0 was first to be added, so it's pointer is null) &mem0 @ @ 0 assertEq
  \ Assert cells 0-7 are zero at &&a1kroot
  0 BEGIN dup cells &&a1kroot @ + @ 0 assertEq 1+ dup 8 < UNTIL drop
  ( &mem39 was last to be added, so it is at &sll1kroot)
  ( 2^A) a1kroot.&sll1k @ @  &mem39 @   assertEq
  ; RUNASTEST
: testA1k.allocPo2=A
  0x A &&a1kroot @ a1k.alloc   1 sysexit
  >R@ ( R@=1k mem) &mem39 @ assertEq
  ( 2^A) a1kroot.&sll1k @ @    &mem39 @ 0x 400 - ( =&mem38)  assertEq

  R> &&a1kroot @ a1k.freeMax
  a1kroot.&sll1k @ @  &mem39 @   assertEq
  ; RUNASTEST
-test


\ TODO:
\ - use 16bit chkId instead of everything else. Use a Vec<&Chk> datastructure held within
\   an allocator to store the type information including moduleId, line num etc. This
\   can easily be serialized/deserialized, so the arena can be dropped when
\   running a BIG program that needs the memory.
\ - This will eliminate FR, F0 will be 
\   NAME 5b | IMM 1b | RES 2b | RES 8b | ChkId 16b
\ - The "interpreter" will execute any IMM words, and will run the typechecker on ANY
\   word that specifies a ChkId. Note that IMM words that want to do their own
\   typechecking themselves (i.e. generics) but still have a ChkId for metadata
\   simply have 0 inp/out types and the generic bit set in those flags.

