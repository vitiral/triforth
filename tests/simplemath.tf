0x 12  0x 3  0x 2 * * 0x 6C assertEq

: simpleMathAnswer 0x 42 ;
simpleMathAnswer 0x 42 assertEq

: double dup + ;
: quadruple double double ;

0x 0 double 0x 0 assertEq
0x 12 double 0x 24 assertEq
0x 4 quadruple 0x 10 assertEq

\ Division
0x A 0x 3 /mod   \ 10 / 3 -> ( remainder quotient )
  0x 3 assertEq  \ quotient
  0x 1 assertEq  \ remainder

\ Multiplication
0x 10 0x 10 * 0x 100 assertEq
assertEmpty

0x 4 4*   0x 10 assertEq
0x 4 16*  0x 40 assertEq
0x 4 2/   0x  2 assertEq
0x 4 4/   0x  1 assertEq

0x 4 0x 42 = assertFalse    0x 4 0x 4 = assertTrue
0x 4 0x 42 <> assertTrue    0x 4 0x 4 <> assertFalse
0x 4 0x 42 < assertTrue     0x 42 0x 4 > assertTrue
0x 4 0x 42 <= assertTrue    0x 42 0x 4 >= assertTrue

0x F000000F invert 0x 0FFFFFF0 assertEq
0x 00FF00 0x F0F0F0 and 0x 00F000 assertEq
0x 00FF00 0x F0F0F0 xor 0x F00FF0 assertEq
0x 00FF00 0x F0F0F0 or  0x F0FFF0 assertEq
0x   8FF1 0x   1008 or  0x 9FF9 assertEq
true  bool true assertEq     false bool false assertEq
0x 42 bool true assertEq     0x 0  bool false assertEq
true not   false assertEq    false not  true assertEq
true =0    false assertEq    false =0   true assertEq

0x 50  0x 40  0x 30  0x 20  0x 10  0x 1
0x 5 pick 0x 50 assertEq   0x 4 pick 0x 40 assertEq
0x 3 pick 0x 30 assertEq   0x 2 pick 0x 20 assertEq
0x 1 pick 0x 10 assertEq   0x 0 pick 0x 01 assertEq
0x 42 0x 5 replace         0x 5 pick 0x 42 assertEq
2drop 2drop 2drop

: testRstack 
  0x 42 0x 50 >R      0x 42 assertEq
  R> 0x 50 assertEq   assertEmpty ;
testRstack

: tval 0x 12345678 ;
\ Test DSP@ with cell and u16 size access
0              DSP@ @              0 assertEq drop
0x 123456      DSP@ @    0x   123456 assertEq drop
0              DSP@ u16@           0 assertEq drop
tval           DSP@ u16@ 0x     5678 assertEq drop
tval         DSP@ 2+ u16@ 0x 1234     assertEq drop
tval DSP@ 0x F0F0 swap 2+ u16! 0x F0F05678 assertEq

0 0x 2 Nshl  0 assertEq         1 0x 5 Nshl 0x 20 assertEq
1 0x 8 Nshl  0x 100 assertEq    1 0x 2 Nshl  0x 4 assertEq
1 0x 10 Nshl  0x 10000 assertEq 4 0x 4 Nshl  0x 40 assertEq
