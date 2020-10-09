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
: true    0x FFFFFFFF ;
: false   0x 0 ;
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

\ The xt's of "!@," repsectively. It can be hard to read otherwise
: !xt ' ! ;     : @xt ' @ ;    : ,xt ' , ;

: [compile] IMM \ ( -- ) immediately compile the next word
  \ This allows you to easily compile IMM words into other words, or compile
  \ words during [ runstate ]
  word find nt>xt \ get xt for next word
  , ;             \ and compile it


\ MARK allows us to define words to be forgotten. It has many uses, but for
\ us the main one is we can now define tests, execute them, and forget
\ everything we learned.

: MARK \ consume next WORD to create dictionary snapshot. When that word is
  \ called, return the dictionary to the previous state.
  &latest @ &here @  \ get the current ( latest here )
  CREATEWORD          \ create the MARK word
  ' lit , ,         \ compile literal HERE directly
  ' &here ,   !xt , \ which the code will store into HERE
  ' lit , ,         \ Same with LATEST
  ' &latest ,   !xt , \ the word will set LATEST to previous value
  ' EXIT , ;

&here @  &latest @
MARK -test
: thisWillNotExist 0x 42 ;
thisWillNotExist 0x 42 assertEq -test
\ -test     \ Neither of these lines will work if uncommented
\ thisWillNotExist
MARK -test  -test \ test mark+unmark works
&latest @ assertEq   &here @ assertEq assertEmpty \ test: dict restored

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

\ BEGIN <block> ( flag ) UNTIL will execute <block> until flag<>0

