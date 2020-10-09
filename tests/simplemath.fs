0x 12  0x 3  0x 2 * * 0x 6C assertEq

: simpleMathAnswer 0x 42 ;
simpleMathAnswer 0x 42 assertEq

: double dup + ;
: quadruple double double ;

0x 0 double 0x 0 assertEq
0x 12 double 0x 24 assertEq
0x 4 quadruple 0x 10 assertEq

\ Division
0x A 0x 3 /mod   \ 10 / 3 -> ( quotient remainder )
  0x 1 assertEq  \ remainder
  0x 3 assertEq  \ quotient

\ Multiplication
0x 10 0x 10 * 0x 100 assertEq
assertEmpty

0x 4 *4   0x 10 assertEq
0x 4 *16  0x 40 assertEq
0x 4 /2   0x  2 assertEq
0x 4 /4   0x  1 assertEq

\ TODO: fix this
\ CREATE simpleVar 0x 10 ,
\ simpleVar @ 0x 10 assertEq
