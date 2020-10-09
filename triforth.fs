\ Now that we have defined our core interpreter in triforth.S we
\ are ready to define the rest of the language.
\
\ Please note that we don't have ( comments ) until we define them,
\ so we will use line comments for everything.

&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode
: /       /mod swap drop ; \ ( a b -- a/b )
: mod     /mod drop ;   \ ( a b -- a%b )
: '\n'    0x A ;
'\n' 0x A assertEq assertEmpty      \ test: '\n' works
&state @ 0x 0 assertEq assertEmpty  \ test: we are in runmode

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
assertEmpty
\ &state @ 0x 0 assertEq          \ test: we are in runmode
\ ':' 0x 3A assertEq assertEmpty  \ test: ':' works, therefore LITERAL, etc works


\ : ';' [ CHAR ; ] LITERAL ; \ ;
\ : '(' [ CHAR ( ] LITERAL ; \ (
\ : ')' [ CHAR ) ] LITERAL ; \ )
\ : '"' [ CHAR " ] LITERAL ; \ "
\ : 'A' [ CHAR A ] LITERAL ; \ A
\ : '0' [ CHAR 0 ] LITERAL ; \ 0
\ : '-' [ CHAR - ] LITERAL ; \ -
\ : '.' [ CHAR . ] LITERAL ; \ .
\ 
\ \ While compiling '[COMPILE]' compiles 'word' if it would otherwise be IMM
\ : [COMPILE] IMM
\   WORD FIND NT>XT \ get xt for next word
\   , ;             \ and compile it
\ 
