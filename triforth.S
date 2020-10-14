# Welcome to the triforth tutorial and programming language.
#
# This programming language and tutorial is created for the Civboot project.
# To learn more about that project visit civboot.org
#
# This tutorial follows from the excellent tutorial "Programming from the ground up"
# chapters 1-5 which gives an excellent introduction to assembly language. Almost
# every assembly instruction here is explained in detail there.
#
# This tutorial draws inspiration from "jonesforth", a tutorial which describes how
# to write a similar forth implementation. I am not an assembly programmer and found
# a little too much hand-waviness in the jonesforth introduction, probably more a
# demonstration of my own lack of knowledge. This tutorial chooses to implement forth
# "naively", sacrificing some performance for performance and defensive programming.
# This tutorial also takes a more "test driven development" (TDD) attitude,
# with tests for each function being written as we write the code. It is hoped
# that this helps the student in playing with code and trying out their own
# implementations.
#
# Go forth and code without fear.

# Why does this language exist?
#
# The first reason is because most useful programming languages are built on
# too much complexity. So much complexity that most students cannot imagine how
# it all started. How DID people program the first C compilers?  Modern C
# compilers are written in C and are so complicated (with their complex
# preprocessor and the amount of standards they must adhere to) that they
# require a binary to bootstrap them, a binary which is completely obtuse and
# immune to understanding from naive students of computers like ourselves.
#
# Understanding doesn't have to be so HARD. The forth compiler has a peculiar ability
# to walk VERY early, after only a few hundred-ish lines of assembly.  In this
# tutorial we will use x86 assembly to bootstrap our own forth language, called
# triforth which is a pun from the "triforce" from the legend of Zelda. Like
# the triforce, triforth will give us the Power to write scalable type-safe
# computer programs, the Knowledge of the inner workings of our language and
# the Courage to deal with the nitty-gritty low level details.

#########################
# Important References:
# `call dbgexit`: write this ANYWHERE (after the first few lines of _start)
# to stop execution and print the stack trace.  Try it now. This can be very
# useful for understanding the function you are reading or writing.
#
# Functions are written
# funcName: # <type signature>
# The type signature is forth-style with input registers in front and output
# registers in back. I.e. 
#
#   $eax:foo ( u:baz u:boz -- dee ) %eax:bob
#
# Takes in %eax and two unsigned ints on the stack. It outputs dee on the stack and
# bob in %eax. In actual fact, most functions either take/put values on the stack
# or take/put registers.
#
# REGISTERS: The below is a helpful register reference.
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx   none  none
# from above 8bit: al ah  bl bh  cl ch  dl dh  none  none
# special:  %esp: stack ptr, our return stack
#           %ebp: base ptr, our data stack
#           %eip: cpu inst ptr
#           %esi: our forth xt-inst ptr
#           %eflags: we won't deal with this
#
# Basic types:
#   SN: N bits data. S is either 'u' for unsigned data or 'i' for a
#     signed integer data. Example: u8 is 8 bits of data, i.e. a "byte" 
#   cell: same as u32, core datatype of rstack and dstack operations.
#   arrSN: some type of array. For example arru8 is an array of u8 values,
#     arru16 is u16 values, etc.
#   carrSN: a counted array. The count value is always in the same type as unsigned type
#     up to the size of a cell (i.e. carru16 has a 16 bit count but carru64
#     still has a 32 bit count) 
#   c2arrS8: an carrS8 that is counted with two bytes instead of 1.
#   c4arrS8: an carrS8 that is counted with four bytes instead of 1.
#   c2arrS16: an carrS16 that is counted with four bytes instead of 2.
#   nt: name token, the POINTER to the first item in the dictionary (link)
#   xt: execution token, the POINTER to the item in a dictionary which itself
#       points to the code to run
#   &T: reference (a.k.a pointer) to type T
#   'W: xt of word W, intended to be executed
#
# TODO: add addressing modes and critical instructions
#

##############################
# Constants
# It's better to define these at the top of the file. Use these as a reference
# as you encounter them.
.equ LINUX_SYSCALL,0x80  # linux kernel interrupt vector
.equ SYS_OPEN, 5
.equ SYS_WRITE, 4
.equ SYS_READ, 3
.equ SYS_CLOSE, 6
.equ SYS_EXIT, 1

# File open options
.equ O_RDONLY, 0x0000  # open for reading only
.equ O_WRONLY, 0x0001  # open for writing only
.equ O_RDWR,   0x0002  # open for reading and writing

# Standard file descriptors
.equ STDIN, 0
.equ STDOUT, 1
.equ STDERR, 2

# Our Return codes
.equ RC_DBG,99 # 99 problems and debugging ain't one, `call dbgexit`
.equ RC_TFAIL,100
.equ RC_UNREACHABLE,101
.equ RC_INVALIDRESET,102
.equ RC_ARRAYBOUNDS,103
.equ RC_PANIC,104
.equ RC_STACK_UNDERFLOW,105
.equ RC_STACK_OVERFLOW,106
.equ RC_STACK_OVERFLOW,106

# Our Panic Error Codes
.equ ERR_SEE_MSG,0x001
.equ ERR_DSTACK_OVERFLOW,0x300
.equ ERR_DSTACK_UNDERFLOW,0x301
.equ ERR_ASCII_BOUNDS,0x302
.equ ERR_MAX_WORD_LEN,0x303
.equ ERR_WORD_MISSING,0x304
.equ ERR_NUMBER_INVALID,0x305
.equ ERR_UNKNOWN_WORD,0x306
.equ ERR_STATE_REDUNDANT,0x307
.equ ERR_STACK_UNDERFLOW,0x308
.equ ERR_WORD_NOT_FOUND,0x309
.equ ERR_INVALID_FLAG,0x310
.equ ERR_INVALID_CHAR,0x311
.equ ERR_STACK_NOT_EMPTY,0x312
.equ ERR_STATE_INCORRECT,0x313
.equ ERR_EMIT_ERR,0x314
.equ ERR_NULL_NAMETOKEN,0x315

.equ FALSE, 0
.equ TRUE, -1

####
# Flags
# We will get into these details when we are constructing the type system.
# The only flags you have to pay attention to for now are:
# NAMELEN: allows backreference from xt->nt
# TIME_MASK and related: specifies when the xt should be run, and whether it
#   has both IMM and RUN components (i.e. : word IMMEDIATE ... DOES> ... EXIT)
#
# The flags are arrayed in this way so that (1) we can have zero-length names
# that take up no space and (2) so that once we have DOES> words (which most
# typed words will be) we only need the right-most (FR) flag cell for lookback.
# Don't worry too much about these details now, but that is why bits are where
# they are.

## Flag 0 (left-most flag)
# Byte 1 (least-significant right-most byte)
.equ F0_RES,               0x000000FF   # 8b: unused
# Byte 2+3
.equ F0_LINENO,            0x00000F00   # 4b: unused
.equ F0_LINENO,            0x00FFF000   # 12b: line number (max 4096)
# Byte 4 (most-significant left-most byte)
.equ F0_NAMELEN_MASK,      0x1F000000   # 5b: name's length (up to 32)
.equ F0_NAMELEN_SHIFT,     24           # na: number of bits to shift
.equ F0_NAMELEN_BYTE_MASK, 0x1F         # na: mask used for left-most byte
.equ F0_RESL,              0xE0000000   # 3b: top 3 bits unused

## FlagR - must be the right-most flags 
# Byte 1
.equ FR_INP_MASK,          0x00000007   # 3b: len input types mask
.equ FR_OUT_MASK,          0x00000038   # 3b: len output types mask
.equ FR_TIME_MASK,         0x000000C0   # 2b: time mask (run/compile/etc)
.equ FR_TIME_RUN,          0x00000000   # na: runtime only word
.equ FR_TIME_IMM,          0x00000040   # na: immediate only word
.equ FR_TIME_IMM_PART,     0x00000080   # na: immediate part of immediate+run
.equ FR_TIME_RUN_PART,     0x000000C0   # na: run part of immediate+run
# Byte 2
.equ FR_IMM_SIZE,          0x0000FF00  # 8b: immediate code size
# Byte 3
.equ FR_RES3,              0x00FF0000  # 8b: reserved for expanding IMM_SIZE
# Byte 4
.equ FR_RES4,              0x0F000000  # 4b: unused bits
.equ FR_TYPE_MASK,         0xF0000000  # 4b: the word's type
.equ FR_TYPE_UNTYPED,      0x00000000  # na: untyped word
.equ FR_TYPE_TYWORD,       0x10000000  # na: typed word
.equ FR_TYPE_STRUCT,       0x20000000  # na: struct (data structure)
.equ FR_TYPE_METHOD,       0x30000000  # na: method (attached typed word_
.equ FR_TYPE_INTERFACE,    0x40000000  # na: interface (collection of methods)
.equ FR_TYPE_MODULE,       0x50000000  # na: module (module-name and file-path)
.equ FR_TYPE_RES,          0x60000000  # na: values 6 to F are unused


######################################################
### DATA SECTION
# It's better to define most .data separately from .text (code), so the
# definitions are here. Refer here when you see the data used in the code.
.section .data
.equ RSTACK_SIZE, 1024
rstackMin: .space RSTACK_SIZE # return stack
rstackMax:
registerSnapshot: .space 24
.equ DSTACK_SIZE, 256
dstackMin: .space DSTACK_SIZE # data stack
dstackMax:
.equ IO_BUFFER_SIZE, 128
iob: .space 1024  # IO buffer, used in fmt\" and other operations.
.equ MAX_WORD_LEN,32
wordBuffer: .space MAX_WORD_LEN
wordCount: .int 0 # for debugging only

# Address that should be called when panicing, can be redirected when testing.
panic: .int defaultPanic
# Specefic error code encountered when panicing.
panicErr: .int 0
# Used in `reset` to return from a panic in testing.
rstackCheckpoint: .int 0

emitFd: .int STDERR # where to EMIT and TYPE to
.equ EMIT_BUFFER_SIZE,16
emitBuffer: .space EMIT_BUFFER_SIZE # scratch space for emit and pntUx
emitBufferMax:
keyFd: .int STDIN   # where KEY/etc gets input from
keyBuffer: .int 0
var_base: .int 10  # used for number conversions

##############################
# Text Strings: unaligned strings for printing messages
testPrefixStr: .ascii "**TEST \0"
testPassStr: .ascii " PASS\n\0"
dbgexitStr: .ascii "\nDBG EXIT\n\0"
panicStr: .ascii "!! PANIC ENCOUNTERED code=\0"
registersAdStr: .ascii "Registers e(a-d)x: \0"
registersDsStr: .ascii "e[ds]i: \0"
wordStr: .ascii "word: \0"
stackStr: .ascii "Stack< \0"
maxWordLenErrorStr: .ascii "Maximum word length exceeded\0"
_0xWordNeededStr: .ascii "Word needed for 0x\0"
_0xNotNumberStr: .ascii "Not a valid hex number: \0"
unknownWordStr: .ascii "Unknown word: \0"
dumpAddrStr: .ascii "DUMP \0"
colonSpaceStr: .ascii ": \0"
errorUnknownEscape: .ascii "ERROR unknown escape: \0"
errorCountExceeded: .ascii "ERROR literal strings must be <255 bytes\0"


# Test Names
testIoBufferStr: .ascii "testIoBuffer\0"
testpntUxStr: .ascii "testpntUx\0"
testPntStackStr: .ascii "testPntStack\0"
testDumpInfoStr: .ascii "testDumpInfo\0"
testNEXTStr: .ascii "testNEXT\0"
testEXITStr: .ascii "testEXIT\0"
testDictSimpleStr: .ascii "testDictSimple\0"
testDictBuiltinStr: .ascii "testDictBuiltin\0"
testKeyStr: .ascii "testKey\0"
testWordStr: .ascii "testWord\0"
test0xStr: .ascii "test0x\0"
testAlign4Str: .ascii "testAlign4\0"
test_ntTOotherStr: .ascii "test_ntTOother\0"
testFindStr: .ascii "testFind\0"
testCreateStr: .ascii "testCreate\0"
testFlagsStr: .ascii "testFlags\0"
testCOLONStr: .ascii "testCOLON\0"
testInterpretStr: .ascii "testInterpret\0"

# Test Files
testSimpleMathFile: .ascii "tests/simplemath.tf\0"
testDefineFile: .ascii "tests/define.tf\0"
testTheAnswerFile: .ascii "tests/theanswer.tf\0"
triforthFile: .ascii "triforth.tf\0"

# Test Inputs
testWordWithSpaces: .ascii " spc    \0"
testAbcStr: .ascii "abc\0"
testBcdStr: .ascii "bcd\0"
testCreatedWord: .ascii "createdword\0"
testCreatedWordUpper: .ascii "CREATEDWORD\0"
testCreatedWordDifferent: .ascii "creat3dword\0"
test3Str: .ascii "3\0"
test3CStr: .ascii "3C\0"
test42Str: .ascii "42\0"
testNewDefinedWordStr: .ascii "NEWDEFINEDWORD\0"

.align 4

######################################################
### CODE SECTION
######################################################
.section .text

##############################
# Core Testing Functions
testSuite: # ( -- )
  # We are doing test driven development. All assembly tests we write should go
  # here. You can ignore them for now.
  call assertEmpty

  call testAsserts
  call testIndirectCall
  call testPanicHandle
  call testDsPushPop
  call testDIVmod
  call testEbxStackSize
  call testDepth
  call testStackBasic
  call assertCleanState

  call testEmit
  call testCountNt
  call testIoBuffer
  call testpntUx
  call testPntStack
  call testMUL
  call testDumpInfo
  call assertCleanState

  call testNEXT
  call testEXIT
  call testDictSimple
  call testBytesEqNoCase
  call assertCleanState

  call testDictBuiltin
  call testLitbytes
  call testKey
  call testWord
  call assertCleanState

  call test0x
  call testConst
  call testAlign4
  call test_ntTOother
  call testFind
  call testCreate
  call testFlags
  call assertCleanState

  call testCOLON
  call assertCleanState

  call testInterpret
  call assertCleanState
  ret

failTest: # The test failed
  call dumpInfo
  movl $RC_TFAIL, %ebx
  jmp sysexit


##############################
# Exiting functions
# These are for quickly exiting our program with relevant return codes.

sysexit: # %ebx:rc ( -- )
  # Directly call SYS_EXIT
  movl $SYS_EXIT, %eax
  int  $LINUX_SYSCALL

unreachable: # ( -- )
  # We call this frequently to practice defensive programming.
  movl $RC_UNREACHABLE, %ebx
  jmp sysexit

exit0: # ( -- )
  # Exits immediately with rc=0
  movl $0, %ebx
  jmp sysexit

assertEaxEbxEq: # %eax %ebx ( -- )
  cmp %eax, %ebx
  jne failTest
  ret

testAsserts:
  # Test that all assertions work as expected
  movl $42, %eax
  movl $42, %ebx
  call assertEaxEbxEq
  ret

testIndirectCall:
  movl $42, %eax
  movl $0, %ebx
  movl $testing_setEbx42, panic
  call *(panic) # indirect call directly to memory
  call assertEaxEbxEq

  movl $0, %ebx
  movl $testing_setEbx42, panic
  movl $panic, %ebx
  call *(%ebx) # indirect call from register

  movl $defaultPanic, panic # reset panic
  movl $0, rstackCheckpoint
  ret
testing_setEbx42:
  movl $42, %ebx
  ret

##############################
# Error Handling Functions
# It might seem odd starting here, but we will almost immediately want to write
# functions which can panic, so we will need to test that behavior to make sure
# we understand that things are working.
#
# Error handling is accomplished via setting a panic catch, which is really
# 1. assigning a panic handler via  `movl $<addr>, panic`
# 2. setting an rstackCheckpoint so `call reset` will return operation after
#    our panicing function.
#
# This would not be adequate in production code, since only one function can
# set a panic catch. We are not going to use this outside of testing however,
# so this restriction is just fine.

reset: # ( * -- ) ( R: * -- )
  # Clears the data and return stacks and returns control at rstackCheckpoint
  # Important: this is ONLY used in panic handling code during testing.
  cmp $0, rstackCheckpoint        # if rstackCheckpoint is 0 then we must exit
                              # since we don't know where to return
  je 1f
  movl rstackCheckpoint, %esp   # reset the rstack to the landing pad
  movl $dstackMax, %ebp            # clear the data stack
  ret                              # return to the top value in new return stack
1:
  movl $RC_INVALIDRESET, %ebx
  jmp sysexit

# A macro to take the checkpoint will be useful.
.macro setPanicCatch a
  movl \a, panic
  leal -4(%esp), %eax  # leal takes the ADDRESS pointed to by the first
  movl %eax, rstackCheckpoint
.endm

resetPanicCatch:
  movl $defaultPanic, panic
  movl $0, rstackCheckpoint
  ret

testPanicHandle: # ( * -- )
  # The first thing we need to do is test the panic handling code we just
  # wrote. The steps are:
  # 1. set up our handling code by setting panic, rstackCheckpoint and panicErr
  # 2. call a function which calls another function, which panics.
  # 3. handle the panic by setting panicErr to 0 and resetting
  # 4. assert panicErr is 0 as the code continues.
  movl $42, panicErr
  setPanicCatch $panicHandle0

  # 2.
  call testing_willPanic
  # 4.
  cmp $0, panicErr
  je 1f
  call failTest
1:
  call resetPanicCatch
  ret
panicHandle0:
  # 3.
  # This is part of this test, but is also the "standard" panic handle for
  # tests. It simply sets panicErr=0
  movl $0, panicErr
  call reset # note: reset returns to addr at rstackCheckpoint
  jmp unreachable
testing_willPanic:
  call testing_panics
  jmp unreachable
testing_panics:
  call *(panic)
  jmp unreachable

##############################
# Stack Operations
#
# The data stack is our primary means of passing data -- both in assembly
# functions and in Forth. This keeps our implementations simpler, as we rarely
# have to think about registers non-locally.
#
# The return stack is the standard assembly one, managed by %esp and the call/ret
# operations. In some cases we may store local variables in it, but that will be
# rare.
#
# Note: All the stacks grow downwards, meaning the top of the stack is at the
# lowest point in memory.
#
# We utilize macros to make the core push/pop functions simpler. We also check
# for over/under flow
#
# Ds: data stack grows downwards bounded by [dstackMin,dstackMax), %ebp is top

checkPushDs:
  # Checks to make sure that the datastack hasn't overflowed.
  # If it has, re-increments and panics with appropriate error.
  cmp $dstackMin, %ebp
  jl 1f
  ret
1:
  addl $4, %ebp
  movl $ERR_DSTACK_OVERFLOW, panicErr
  call *(panic)
  jmp unreachable

.macro dpush reg
  subl $4, %ebp
  call checkPushDs
  movl \reg , (%ebp)
.endm

checkPopDs:
  # Checks to make sure that the datastack hasn't underflowed.
  # If it has, re-decrements and panics with appropriate error.
  cmp $dstackMax, %ebp
  jge 1f
  ret
1:
  subl $4, %ebp
  movl $ERR_DSTACK_UNDERFLOW, panicErr
  call *(panic)
  jmp unreachable


.macro dpop reg
  movl (%ebp) , \reg
  call checkPopDs
  addl $4, %ebp
.endm

assertEmpty:
  # Assert that the datastack is empty
  movl %ebp, %eax
  movl $dstackMax, %ebx
  call assertEaxEbxEq
  ret

testDsPushPop:
  # test push/pop and overflow/underflow checking
  call assertEmpty

  # push/pop works
  dpush $42
  dpop %eax
  movl $42, %ebx
  call assertEaxEbxEq
  call assertEmpty

  # push2 / pop2 works
  dpush $43
  dpush $44
  dpop %eax
  movl $44, %ebx
  call assertEaxEbxEq
  dpop %eax
  movl $43, %ebx
  call assertEaxEbxEq
  call assertEmpty

  movl $77, panicErr
  setPanicCatch $panicHandle0
  call testing_underflow
  cmp $0, panicErr
  jne testDsPushPop_fail

  movl $77, panicErr
  setPanicCatch $panicHandle0
  call testing_overflow
  cmp $0, panicErr
  jne testDsPushPop_fail

  call resetPanicCatch
  call assertCleanState
  ret
testDsPushPop_fail:
  call failTest
testing_underflow:
  call assertEmpty
  dpush $4
  dpop %eax
  dpop %eax # underflow
  call unreachable
testing_overflow:
  call assertEmpty
  movl $dstackMin, %ebp
  addl $4, %eax # overflows when <=, so will still overflow 4 above.
  dpush $4  # overflow
  call unreachable

DIVmod: # ( u:a u:b -- u:rem u:quot )
  # Forth MOD/, unsigned divide a by b
  # returns: quotient remainder
  dpop %ebx # divisor
  movl $0, %edx
  dpop %eax # %edx:%eax=number

  divl %ebx  # %edx:%eax / %ebx => %eax=quotient %edx=remainder
  dpush %edx # remainder
  dpush %eax # quotient
  ret

testDIVmod:
  dpush $13
  dpush $5
  call DIVmod
  dpop %eax
  movl $2, %ebx # quotient
  call assertEaxEbxEq

  dpop %eax
  movl $3, %ebx # remainder
  call assertEaxEbxEq
  call assertEmpty
  ret

.macro ebxStackSize # set ebx to the stack size
  movl $dstackMax, %ebx
  subl %ebp, %ebx  # stackSize=ebx=dstackMax - dstackPtr
.endm

testEbxStackSize:
  # going to check the math
  ebxStackSize
  movl $0, %eax
  call assertEaxEbxEq

  dpush $42
  ebxStackSize
  movl $4, %eax
  call assertEaxEbxEq

  dpush $42
  ebxStackSize
  movl $8, %eax
  call assertEaxEbxEq

  # Here we write checkStackSize verbatum, since we can't test it directly
  ebxStackSize
  movl $4, %eax
  cmpl %ebx, %eax
  jg 1f # error

  dpop %eax
  dpop %eax
  ebxStackSize
  movl $0, %eax
  call assertEaxEbxEq
  ret
1:
  jmp sysexit


checkStackSize: # %eax:needSize ( )
  # If stack size is not >=eax, panic
  ebxStackSize
  cmpl %ebx, %eax
  jg 1f
  ret
1:
  movl $ERR_STACK_UNDERFLOW, panicErr
  call *(panic)

swap: # ( a b -- b a )
  # Forth SWAP
  movl $8, %eax
  call checkStackSize
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %eax
  ret

dup: # ( a -- a a )
  movl $4, %eax
  call checkStackSize
  movl (%ebp), %eax
  dpush %eax
  ret

_2dup: # ( a b -- a b a b )
  movl $8, %eax
  call checkStackSize
  movl 4(%ebp), %eax
  movl (%ebp), %ebx
  dpush %eax
  dpush %ebx
  ret

drop: # ( a -- )
  movl $4, %eax
  call checkStackSize
  addl $4, %ebp
  ret

_2drop: # ( a b -- )
  movl $8, %eax
  call checkStackSize
  addl $8, %ebp
  ret

_3drop: # ( a b c -- )
  movl $12, %eax
  call checkStackSize
  addl $12, %ebp
  ret

_4drop: # ( a b c d -- )
  movl $16, %eax
  call checkStackSize
  addl $16, %ebp
  ret

lrot: # ( a b c -- b c a )
  # Forth ROT, rotates left
  movl $12, %eax
  call checkStackSize
  dpop %ecx
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %ecx
  dpush %eax
  ret

rrot: # ( a b c -- c a b )
  # Forth -ROT, rotates right
  movl $12, %eax
  call checkStackSize
  dpop %ecx
  dpop %ebx
  dpop %eax
  dpush %ecx
  dpush %eax
  dpush %ebx
  ret

over: # ( a b -- a b a )
  movl $8, %eax
  call checkStackSize
  movl 4(%ebp), %eax
  dpush %eax
  ret

nip: # ( a b -- b ) \ drops item below top of stack
  dpop %ebx
  dpop %eax
  dpush %ebx
  ret

depth: # ( -- u )
  # returns the depth of the stack
  ebxStackSize
  shr  $2, %ebx # divide by 4
  dpush %ebx
  ret

testDepth:
  dpush $1
  dpush $2
  call depth
  dpop %eax
  movl $2, %ebx
  call assertEaxEbxEq

  call drop
  call depth
  dpop %eax
  movl $1, %ebx
  call assertEaxEbxEq

  call drop
  call depth
  dpop %eax
  movl $0, %ebx
  call assertEaxEbxEq
  ret

testStackBasic:
  dpush $2
  dpush $1
  dpush $0 # ( 2 1 0)
  call swap # ( 2 0 1 )

  movl (%ebp), %eax
  movl $1, %ebx
  call assertEaxEbxEq
  movl 4(%ebp), %eax
  movl $0, %ebx
  call assertEaxEbxEq

  call lrot # ( 0 1 2 )

  movl (%ebp), %eax
  movl $2, %ebx
  call assertEaxEbxEq
  movl 4(%ebp), %eax
  movl $1, %ebx
  call assertEaxEbxEq
  movl 8(%ebp), %eax
  movl $0, %ebx
  call assertEaxEbxEq

  call rrot # ( 2 0 1 )

  movl (%ebp), %eax
  movl $1, %ebx
  call assertEaxEbxEq
  movl 4(%ebp), %eax
  movl $0, %ebx
  call assertEaxEbxEq
  movl 8(%ebp), %eax
  movl $2, %ebx
  call assertEaxEbxEq

  call _2drop
  call drop
  ret

##############################
# String and Emiting/Printing
# Dealing with strings and outputing to STDERR is important when devleoping and
# debugging. The below functions allow easier access to data than printing a
# return code, including pntStack which allows us to view the stack at any
# time.
# In addition, many of these will eventually be included directly as forth
# words, so by defining them as early as possible we can get the greatest use
# of them.

emit: # ( c -- )
  # emits a single byte to emitFd
  dpop %eax
  movb %al, emitBuffer
  movl emitFd, %ebx   # fd
  movl $emitBuffer, %ecx # &buf
  movl $1, %edx       # bytesToWrite
syswrite: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
  # Direct call to SYS_WRITE syscall with error checking.
  movl $SYS_WRITE, %eax
  int $LINUX_SYSCALL
  cmp $0, %eax
  jl 1f
  ret
1:
  movl %eax, panicErr
  call *(panic)
  jmp unreachable

testEmit:
  dpush $'E'
  call emit
  dpush $' '
  call emit
  dpush $'P'
  call emit
  dpush $'A'
  call emit
  dpush $'S'
  call emit
  dpush $'S'
  call emit
  dpush $'\n'
  call emit
  ret

countnt: # ( addr -- addr count )
  # Given a null-terminated (c-style) string re-returns the string address and
  # the count
  movl $4, %eax
  call checkStackSize
  movl (%ebp), %eax # addr
  movl $0, %ebx # count
1:
  cmpb $0, (%eax,%ebx,1)
  je 2f # exit loop
  inc %ebx
  jmp 1b # loop
2:
  dpush %ebx
  ret

testCountNt:
  dpush $testPassStr
  call countnt
  dpop %ebx
  movl $6, %eax
  call assertEaxEbxEq
  call drop
  ret

pntS: # ( addr count -- )
  # Forth TYPE. Writes count bytes at addr to emitFd
  movl emitFd, %ebx   # fd
  dpop %edx # bytesToWrite=count
  dpop %ecx # &buf
  call syswrite
  ret

pntTestPass: # ( addr count -- )
  # pnts "TEST <addr count> PASS\n" to emitFd
  dpush $testPrefixStr
  call countnt
  call pntS
  call pntS
  dpush $testPassStr
  call countnt
  call pntS
  ret

pntSNt: # ( addr -- )
  # Type a null nerminated str
  call countnt
  call pntS
  ret

pntSNtln: # ( addr -- )
  # Type a null nerminated str ending w/ newline
  call pntSNt
  dpush $'\n'
  call emit
  ret

testIoBuffer:
  dpush $testIoBufferStr
  call countnt
  call pntTestPass
  ret

bSTORE: # ( b addr -- )
  # Store byte at address
  dpop %eax  # addr
  dpop %ebx  # c
  movb %bl, (%eax)
  ret

pntUx: # ( n -- ) \ emit an unsigned hex number
  movl $4, %eax
  call checkStackSize
  cmpl $0, (%ebp) # if number if 0 just print that
  je pntUx_zero
  dpush $0
  call swap # ( count n )
pntUx_loop:
  movl (%ebp), %ebx
  cmpl $0, %ebx  # if number is zero, exit
  je pntUx_ret
  andl $15, %ebx  # ebx=ebx%16
  shrl $4, (%ebp) # divide n by 16. Note: n>>4 == n/2^4
  call ebx_toascii   # ( count n )
  # eax=emitBufferMax - 1 - count
  movl $emitBufferMax-1, %eax 
  subl 4(%ebp), %eax
  dpush %ebx          # byte to store
  dpush %eax          # address to store byte at
  call bSTORE
  addl $1, 4(%ebp)    # increment count
  jmp pntUx_loop
pntUx_ret:
  call drop # drop n (it is 0)
  movl $emitBufferMax, %eax 
  subl (%ebp), %eax
  dpush %eax
  call swap
  call pntS
  dpush $' '
  call emit
  ret
pntUx_zero:
  call drop
  dpush $'0'
  call emit
  dpush $' '
  call emit
  ret

ebx_toascii: # ( u -- c )
  # convert a number (typically 1-16) into ascii equivalent
  # This is used with pntUx (above)
  cmp $9, %ebx
  jg 1f
  # number <= 9
  addl $'0', %ebx
  jmp 2f
1: # number > 9
  subl $10, %ebx
  cmpl $23, %ebx
  jg 3f
  addl $'A', %ebx
2: # push and return
  ret
3: # error
  movl $ERR_ASCII_BOUNDS, panicErr
  call *(panic)
  call unreachable

testpntUx:
  dpush $1
  call pntUx
  dpush $0x12
  call pntUx
  dpush $0xDEADBEEF
  call pntUx
  dpush $'\n'
  call emit

  dpush $testpntUxStr
  call countnt
  call pntTestPass
  call assertEmpty
  ret

MUL: # ( a b -- u )
  # Forth *, multiply the two numbers
  dpop %eax
  dpop %ecx
  mull %ecx # %eax=%eax*%ecx
  dpush %eax
  ret

testMUL:
  dpush $8
  dpush $2
  call MUL
  dpop %eax
  movl $16, %ebx
  call assertEaxEbxEq
  ret

pntStack: # ( -- )
  # Forth .S, prints the current stack
  dpush $stackStr
  # "Stack< DEPTH > "
  call pntSNt

  call depth 
  call pntUx

  dpush $'>'
  call emit
  dpush $' '
  call emit

  call depth # 4(%ebp) is depth
  dpush $0   # (%ebp) is index
1:
  movl (%ebp), %eax
  cmp %eax, 4(%ebp) # compare depth with index
  je 2f
  call dup
  dpush $4
  call MUL 
  dpop %eax # index * 4
  movl $dstackMax, %ebx
  subl $4, %ebx   # first item is under dstackMax
  subl %eax, %ebx # ebx=dstackMax - 4 - index*4

  # print the number in hex
  movl (%ebx), %eax
  dpush %eax
  call pntUx

  addl $1, (%ebp) # increment index
  jmp 1b # loop

2:
  call _2drop
  dpush $'\n'
  call emit
  ret

testPntStack:
  dpush $0x01
  dpush $0x02
  dpush $0xAA
  dpush $0x1A2B
  call pntStack
  call _2drop
  call _2drop
  dpush $testPntStackStr
  call countnt
  call pntTestPass
  ret

bmove: # ( addr1 addr2 count -- )
  # Forth BMOVE, move count bytes from addr1 -> addr2.
  dpop %eax # count
  dpop %ecx # addr2
  dpop %ebx # addr1
  movl $0, %edi  # index
1:
  cmp %edi, %eax
  je 2f
  movb (%ebx,%edi,1), %dl
  movb %dl, (%ecx,%edi,1)
  inc %edi
  jmp 1b
2:
  ret

bmoveUpper: # ( addr1 addr2 count -- )
  # Move count bytes from addr1 -> addr2 as uppercase
  dpop %ecx # count
  dpop %edx # addr2
  dpop %eax # addr1
  movl $0, %edi  # index
1:
  cmp %edi, %ecx
  je 2f
  movb (%eax,%edi,1), %bl
  call bl_toupper
  movb %bl, (%edx,%edi,1)
  inc %edi
  jmp 1b
2:
  ret

##############################
# Debug Helpers
# These will help us as we develop to view the state of the stack and registers
# anywhere in our program. They will also give us more helpful error messages
# when things fail.

snapshotRegisters:
  movl %eax, registerSnapshot
  movl $registerSnapshot, %eax
  movl %ebx, 4(%eax)
  movl %ecx, 8(%eax)
  movl %edx, 12(%eax)
  movl %edi, 16(%eax)
  movl %esi, 20(%eax)
  ret

restoreRegisters:
  movl $registerSnapshot, %eax
  movl 4(%eax), %ebx
  movl 8(%eax), %ecx
  movl 12(%eax), %edx
  movl 16(%eax), %edi
  movl 20(%eax), %esi
  movl registerSnapshot, %eax
  ret

dumpInfo:
  call snapshotRegisters

  # Registers
  dpush $registersAdStr
  call pntSNt
  movl $registerSnapshot, %ebx
  movl (%ebx), %eax  # eax
  dpush %eax
  call pntUx

  movl $registerSnapshot, %ebx
  movl 4(%ebx), %eax # ebx
  dpush %eax
  call pntUx

  movl $registerSnapshot, %ebx
  movl 8(%ebx), %eax # ecx
  dpush %eax
  call pntUx

  movl $registerSnapshot, %ebx
  movl 12(%ebx), %eax # edx
  dpush %eax
  call pntUx

  movl $registerSnapshot, %ebx
  dpush $registersDsStr
  call pntSNt
  movl $registerSnapshot, %ebx
  movl 16(%ebx), %eax # edi
  dpush %eax
  call pntUx

  movl $registerSnapshot, %ebx
  movl 20(%ebx), %eax # esi
  dpush %eax
  call pntUx
  dpush $'\n'
  call emit

  # Stack
  call pntStack
  # current word
  dpush $wordStr
  call pntSNt
  dpush $wordBuffer
  movl wordCount, %eax
  dpush %eax
  call pntS
  dpush $'\n'
  call emit

  call restoreRegisters
  ret

testDumpInfo:
  # preload iob with something
  dpush $testDumpInfoStr
  call countnt # ( addr count )
  movl (%ebp), %eax
  movl %eax, wordCount
  dpush $wordBuffer
  call swap # ( addr1 addr2 count )
  call bmove

  # push some values onto the stack
  dpush $0xAB
  dpush $0xAC

  movl $0x01, %eax
  movl $0x02, %ebx
  movl $0x03, %ecx
  movl $0x04, %edx
  movl $0x10, %edi
  movl $0x11, %esi
  call dumpInfo

  call _2drop
  movl $0, wordCount
  dpush $testDumpInfoStr
  call countnt
  call pntTestPass
  ret

dbgexit: # ( -- )
  # Exits immediately with rc=99
  # Mainly used in debugging.
  call dumpInfo
  dpush $dbgexitStr
  call pntSNt
  movl $RC_DBG, %ebx
  jmp sysexit

defaultPanic: # ( -- )
  call dumpInfo
  dpush $panicStr
  call pntSNt
  movl panicErr, %eax
  dpush %eax
  call pntUx
  dpush $'\n'
  call emit
  movl $RC_PANIC, %ebx
  jmp sysexit


##############################
# Forth Interpreter
# Almost the entire forth "interpreter" is a NEXT macro in a loop. The NEXT macro
# is literally two lines of assembly. What does it do?
#
# All the forth interpreter does is execute a string of xt's (execution
# tokens). The xt to be executed will be stored in %esi, with return xt's
# stored on the normal return stack with pushl and popl. xt's are nothing more
# than double pointers to assembly instructions to execute, with one caveat:
# Those assembly instructions must end in NEXT instead of ret. NEXT means that
# the assembly will run the next instruction in the xt-list, which will run the
# next, etc. Assembly code that calls forth code then puts it's own
# continuation location on the xt stack to return execution to itself.
#
# testNEXT demonsrates the basic principle of executing words written in
# assembly, but doesn't get to the interesting part -- how are forth words
# defined in terms of other forth words? To do that we need DOCOL and EXIT.
# DOCOL ("do colon") is what every "standard" forth word's xt points to. All it
# does is store the current xt on the return stack and call NEXT on it's own
# xt's. All forth words then end in EXIT, which simply pops the value on the
# rstack into %esi and calls NEXT.
#
# See how they all work together in testEXIT. Keep in mind that xt's are themselves
# pointers to code, meaning that a list of xt's is a list of double pointers.
# Each item in the list points to an xt which ITSELF points to the code to execute.
# This can be confusing, but it allows forth words to not have to define their
# own executible code, most can simply point to DOCOL.

.macro NEXT
  # Note that lodsl is the same as below:
  # movl (%esi), %eax
  # addl $4, %esi
  lodsl  # %eax=(%esi) %esi+=4
  jmp *(%eax)
.endm

.align 4
DOCOL:
  pushl %esi      # push esi (xt) onto the return stack
  addl $4, %eax   # %eax points to xt (from NEXT)
  movl %eax, %esi # so make esi point to the first word to execute
  NEXT

word_EXIT:
  popl %esi # pop return xt into esi
  NEXT      # jmp to it and increment esi
fake_xt_EXIT: # here for testing/demo. The real xt will be in the dict
  .int word_EXIT
  .align 4

word_MUL: # ( u u -- u )
  # Multiply two numbers on the stack. Note: this is our first "true" forth
  # word. All forth words will be prefixed with `word_` and must end with NEXT
  # instead of `ret`.
  call MUL
  NEXT
fake_xt_MUL: # here for testing/demo. The real xt will be in the dict
  .int word_MUL
  .align 4

testNEXT:
  # In this test we will store our fake "xt's" in the iob, since we know
  # it isn't being used.
  movl $iob, %esi
  # Forth: 7 3 2 * * testNEXT_xt
  dpush $7
  dpush $3
  dpush $2
  movl $fake_xt_MUL, (%esi)
  movl $fake_xt_MUL, 4(%esi)
  movl $testNEXT_xt, 8(%esi)
  # DOCOL will push %esi onto the return stack and increment %eax to point to
  # the xt's to execute, each of which will call NEXT on their own until EXIT
  # returns to the value on the return stack.
  NEXT
  # Note the indirection that is inherent to everyting on the xt stack.
testNEXT_xt : # this "mimicks" a forth word execution token.
  .int testNEXT_cont
  .align 4
testNEXT_cont:
  dpop %eax
  movl $42, %ebx
  call assertEaxEbxEq
  dpush $testNEXTStr
  call countnt
  call pntTestPass
  ret

word_pushI2:
  dpush $2
  NEXT
xt_pushI2:
  .int word_pushI2
  .align 4

_callXt: # %eax:xt ( * -- * )
  # Executes an xt, returning control when complete.
  # This can be used to execute forth words from within asm.
  pushl %esi  # store current esi so it can be restored
  movl $callXt_exit, %esi # set the "currently executing" to own control flow
  jmp *(%eax)
.align 4
# The double indirection is there because of how DOCOL and EXIT work together.
callXt_exit: .int callXt_xt # kind of a "fake exit"
.align 4
callXt_xt: .int callXt_cont # kind of a "fake xt"
callXt_cont:
  popl %esi # restore esi
  ret

.macro callXt value
  movl \value , %eax
  call _callXt
.endm

testEXIT:
  movl $iob, %ebx

  # DBL
  movl $DOCOL, (%ebx)
  movl $xt_pushI2, 4(%ebx)
  movl $fake_xt_MUL, 8(%ebx)
  movl $fake_xt_EXIT, 12(%ebx)

  # QUAD
  movl $DOCOL, 16(%ebx)
  movl %ebx, 20(%ebx)  # DBL
  movl %ebx, 24(%ebx)  # DBL
  movl $fake_xt_EXIT, 28(%ebx)

  dpush $8  # put 8 on the stack

  # movl $iob+16, %eax  # QUAD xt
  callXt $iob+16

  dpop %eax
  movl $32, %ebx
  call assertEaxEbxEq

  dpush $testEXITStr
  call countnt
  call pntTestPass
  ret

dump: # ( addr count -- ) \ Dump the contents of the address onto emitFd
  call snapshotRegisters
  dpush $dumpAddrStr
  call pntSNt
  call over
  call pntUx
  dpush $'\n'
  call emit

  # Make sure we are always starting at an aligned address
  call over
  callXt $xt_align4
  dpop %eax
  cmp 4(%ebp), %eax
  je 1f
  subl $4, %eax # we are not aligned, start at aligned-4
  movl %eax, 4(%ebp)
1:
  dpop %eax # count
  addl (%ebp), %eax  # addr+count=address we want to get to
  dpush %eax
2:
  movl (%ebp), %eax
  cmp %eax, 4(%ebp) # does addr=addrtop?
  je 2f
  call over
  call dump_addr
  addl $4, 4(%ebp) # add 4 to addr
  jmp 2b
2:
  call _2drop
  call restoreRegisters
  ret

dump_addr: # ( addr -- ) \ print "<addr>: "
  call dup
  call pntUx
  dpush $colonSpaceStr
  call pntSNt
  callXt $xt_FETCH
  call pntUx
  dpush $'\n'
  call emit
  ret


##############################
# Forth Dictionary
# The forth dictionary is our most important datatype. Fundamentally it is
# singly-linked list. This is a fundamentally simple datatype where each "item"
# within the list contains a link to the previous item, like this:
#
#     ...
#     link | ... data ...
#      ^
#      |
#     link | ... data ...
#
# The dictionary in our forth (and most forths) is a linked list, and the data is
# arrayed like this:
#
#         ...
#        link | name | info | xt | code/etc ...
#         ...
# 
# The various fields above are;
# - link: this is the link to the previous entry in the LL, or 0 if this is the end. A pointer
#   to this is called the nt (name token) because it's easy to find the name from it. This is
#   what FIND returns.
# - name: a ctstr (counted str) with the count as the first byte and the string as the rest.
#    we pad this string so it aligns with 4 bytes.
# - info: N bytes of information containing:
#   - prescedence bit: whether this word should be executed at compile time.
#   - TODO: flush this out
# - xt: the "execution token", see previous section. Points to the code to execute in order
#   to run this code.
# - code/etc: the code and data that follows the xt will depend on the word. We will get into
#   more detail later.
#
# We have already nearly defined a few words, so let's start the dictionary!

.set LINK, 0  # this will keep track of the "previous" link for us.

# ( -- ) \ exit from word
.section .rodata
.align 4
nt_EXIT:
  .int LINK           # link to previous (in this case 0)
  .set LINK, nt_EXIT  # set link for next word to be defined
c8bytes_EXIT:
  .byte 4       # name length
  .ascii "EXIT" # the word name
  .align 4
flags_EXIT:
  .int (4<<F0_NAMELEN_SHIFT)  # F0
  .int 0                      # FR
xt_EXIT:
  .int word_EXIT # xt

# ( a b -- a*b ) \ multiply
nt_MUL:
  .int LINK            # link to previous (in this case 0)
  .set LINK, nt_MUL  # set link for next word to be defined
c8bytes_MUL:
  .byte 1       # name length
  .ascii "*"    # the word name
  .align 4
flags_MUL:
  .int (3<<F0_NAMELEN_SHIFT)  # F0
  .int 0                      # FR
xt_MUL:
  .int word_MUL # xt

testDictSimple:
  movl nt_EXIT, %eax
  movl $0, %ebx
  call assertEaxEbxEq

  dpush $7  # put 8 on the stack
  dpush $3  # put 8 on the stack
  dpush $2  # put 8 on the stack
  # Forth= : * * ;
  movl $iob, %ebx
  movl $DOCOL, (%ebx)
  movl $xt_MUL, 4(%ebx)
  movl $xt_MUL, 8(%ebx)
  movl $xt_EXIT, 12(%ebx)
  callXt $iob

  dpop %eax
  movl $42, %ebx
  call assertEaxEbxEq
  dpush $testDictSimpleStr
  call countnt
  call pntTestPass
  ret

##############################
# Builtin Words
# Now that we have the basic structure of the dictionary, we are going to
# define several words using the below macro.
#
# To use the macro we simply call it and then write the assembly that should go
# in the word. Some of these we have already defined, tested and used in
# implementing other core words, in which case will simply call our
# implementations.
.macro def_asmword name , namelen , flags=0 , label
  .align 4
nt_\label :
  .int LINK            # link to previous
  .set LINK,nt_\label  # update link to self
c8bytes_\label :
  .byte \namelen       # the length of the name
  .ascii "\name"       # the ascii name itself
  .align 4             # padding to next 4 byte boundary
flags_\label :
  .int \namelen << F0_NAMELEN_SHIFT # F0
  .int \flags                       # FR
xt_\label :
  .int word_\label
word_\label :
.endm

# Since we have already written so many of the funcitons we need, we will write
# a second macro. All it does is call the function we have already written.

.macro def_callword name, namelen, flags, label
def_asmword \name , \namelen , \flags , \label
  call \label
  NEXT
.endm

# Finally, this macro allows for defining a word in terms of calling other
# words.
.macro def_doword name , namelen , flags=0 , label
  .align 4
nt_\label :
  .int LINK             # link to previous
  .set LINK,nt_\label   # update link to self
c8bytes_\label :
  .byte \namelen        # the length of the name
  .ascii "\name"        # the ascii name itself
  .align 4              # padding to next 4 byte boundary
flags_\label :
  .int \namelen << F0_NAMELEN_SHIFT # F0
  .int \flags                       # FR
xt_\label :
  .int DOCOL
# follow with .int for xt's to call
.endm

# ( a b -- b a )
#           ascii ,  len, flag, label
def_callword "SWAP" , 4 ,  0 ,   swap

# ( a -- a a )
#           ascii , len, flag, label
def_callword "DUP" , 3 ,  0 ,   dup

# ( a b -- a b a b )
#           ascii , len, flag, label
def_callword "2DUP" , 4 ,  0 ,   _2dup

# ( a b c -- a b c a b c )
#           ascii , len, flag, label
def_asmword "3DUP" , 4 ,  0 ,   _3dup
  movl $12, %eax
  call checkStackSize
  movl 8(%ebp), %eax
  movl 4(%ebp), %ebx
  movl (%ebp), %ecx
  dpush %eax
  dpush %ebx
  dpush %ecx
  ret

# ( a -- )
#           ascii ,  len, flag, label
def_callword "DROP" , 4 ,  0 ,   drop

# ( a b -- )
#           ascii ,  len, flag, label
def_callword "2DROP" , 5 ,  0 ,   _2drop

# ( a b c -- b c a ) \ rotates left
#           ascii ,  len, flag, label
def_callword "LROT" , 4 ,  0 ,   lrot

# ( a b c -- c a b ) \ rotates right
#           ascii  , len, flag, label
def_callword "RROT" , 4 ,  0 ,   rrot

# ( a b -- a b a )
#           ascii  , len, flag, label
def_callword "OVER" , 4 ,  0 ,   over

# ( a b -- b ) \ drops item below top of stack
#           ascii , len, flag, label
def_callword "NIP" , 3 ,  0   , nip

tuck: # ( a b -- b a b ) \ copy the top stack item below the second stack item
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %eax
  dpush %ebx
  ret

# ( a b -- b a b ) \ copy the top stack item below the second stack item
#            ascii  , len, flag, label
def_callword "TUCK" , 4 ,  0 ,   tuck

# ( i -- u ) \ fetch the index of the stack.
#            ascii  , len, flag, label
def_asmword "PICK" , 4 ,  0 ,   pick
  dpop %eax   # index
  shl  $2, %eax # multiply by 4
  call checkStackSize # check that access is valid
  addl %ebp, %eax
  movl (%eax), %eax
  dpush %eax
  NEXT

# ( u i -- ) \ store value at the index on the stack (minus these params)
#              ascii  , len, flag, label
def_asmword "REPLACE" ,   7,    0 , replace
  dpop %eax   # index
  dpop %ecx   # value, don't use ebx because checkStackSize
  shl  $2, %eax # multiply by 4 (cell size)
  call checkStackSize # check that access is valid
  addl %ebp, %eax
  movl %ecx, (%eax)
  NEXT

# ( a -- a a? ) \ duplicate top of stack if non-zero
#           ascii  , len, flag, label
def_asmword "?DUP" , 4 ,  0 ,   QDUP
  movl $4, %eax
  call checkStackSize
  cmp $0, (%ebp)
  je 1f
  call dup
1:
  NEXT

# ( u -- ) \ put a value onto the return stack
#         ascii , len, flag, label
def_asmword ">R",   2,    0,  rpush
  dpop %eax
  pushl %eax
  NEXT

# ( -- u ) \ pop a value from the return stack
#         ascii , len, flag, label
def_asmword "R>",   2,    0,  rpop
  popl %eax
  dpush %eax
  NEXT

# ( -- addr ) \ get the return stack pointer
#         ascii   , len, flag, label
def_asmword "RSP@",   4,    0,  rstackFETCH
  dpush %esp
  NEXT

# ( addr -- ) \ set the return stack pointer
#         ascii   , len, flag, label
def_asmword "RSP!",   4,    0,  rstackSTORE
  dpop %esp
  NEXT

# ( -- addr ) \ get the data stack pointer
#         ascii   , len, flag, label
def_asmword "DSP@",   4,    0, dstackFETCH
  movl %ebp, %eax
  dpush %eax
  NEXT

# ( addr -- * ) \ set the data stack pointer
#         ascii   , len, flag, label
def_asmword "DSP!",   4,    0, dstackSTORE
  dpop %eax
  movl %eax, %ebp
  NEXT

# ( -- u ) \ depth of the stack
#           ascii   , len, flag, label
def_callword "DEPTH" , 5 ,  0 ,   depth

# ( u addr -- ) \ stores the value at address
#         ascii , len, flag, label
def_asmword "!" ,   1,    0,  STORE
  dpop %eax # addr
  dpop %ebx # u
  movl %ebx, (%eax)
  NEXT

# ( addr -- u ) \ fetches the value at address
#         ascii , len, flag, label
def_asmword "@" ,  1 ,  0 ,  FETCH
  dpop %eax
  movl (%eax), %ebx
  dpush %ebx
  NEXT

# ( addr -- ) \ increments the value at address
#           ascii , len, flag, label
def_asmword "+!" ,  2 ,  0 ,  ADDSTORE
  dpop %eax
  addl $1, (%eax)
  NEXT

# ( addr -- ) \ decrements the value at address
#           ascii , len, flag, label
def_asmword "-!" ,  2 ,  0 ,  SUBSTORE
  dpop %eax
  subl $1, (%eax)
  NEXT

# ( u8 addr -- ) \ stores a byte at address
#           ascii , len, flag, label
def_callword "B!" ,  2 ,  0 ,  bSTORE

# ( u16 addr -- ) \ store 16bit value at address
#         ascii , len, flag, label
def_asmword "U16!" ,  4 ,  0 ,  u16STORE
  dpop %eax # addr
  dpop %ebx # value
  movw %bx, (%eax)
  NEXT

# ( addr -- u8 ) \ fetches a byte at address
#           ascii , len, flag, label
def_asmword "B@" ,  2 ,  0 ,  bFETCH
  dpop %eax # addr
  movl $0, %ebx
  movb (%eax), %bl
  dpush %ebx
  NEXT

# ( addr -- u16 ) \ fetches 16bit value at address
#         ascii , len, flag, label
def_asmword "U16@" ,  4 ,  0 ,  u16FETCH
  dpop %eax
  movl $0, %ebx
  mov (%eax), %bx
  dpush %ebx
  NEXT

# ( n -- ) \ prints an unsigned integer as hex
#           ascii, len, flag, label
def_callword ".ux" ,  3 ,  0 ,  pntUx

# ( -- ) \ fail test
#                 ascii, len, flag, label
def_callword "FAILTEST",  8,    0, failTest

# ( flag -- ) \ true or fail test
#           ascii,        len, flag, label
def_asmword "ASSERTTRUE",  10,    0, assertTrue
  dpop %eax
  cmp $0, %eax
  je 1f
  NEXT
1:
  call failTest
  call unreachable

# ( flag -- ) \ false or fail test
#           ascii,         len, flag, label
def_asmword "ASSERTFALSE",  11,    0, assertFalse
  dpop %eax
  cmp $0, %eax
  jne 1f
  NEXT
1:
  call failTest
  call unreachable

# ( u u -- ) \ equal or fail test
#           ascii,     len, flag, label
def_asmword "ASSERTEQ",  8,    0, assertEq
  dpop %eax
  dpop %ebx
  call assertEaxEbxEq
  NEXT

# ( -- ) \ asserts that stack is empty
#                    ascii,len, flag, label
def_callword "ASSERTEMPTY", 11,  0, assertEmpty

# ( addr -- ) \ assert the address is aligned by 4
#                     ascii,     len, flag, label
def_asmword "ASSERTALIGN4", 13, 0, assertAlign4
  dpop %eax
  andl $3, %eax
  movl $0, %ebx
  call assertEaxEbxEq
  NEXT

assertCleanState:
  call assertEmpty

  movl panic, %eax
  movl $defaultPanic, %ebx
  call assertEaxEbxEq

  movl panicErr, %eax
  movl $0, %ebx
  call assertEaxEbxEq

  movl rstackCheckpoint, %eax
  movl $0, %ebx
  call assertEaxEbxEq

  callXt $xt_REFhere
  callXt $xt_assertAlign4
  ret

# ( -- ) \ assert the state is clean
#                     ascii,     len, flag, label
def_callword "ASSERTCLEANSTATE", 16, 0, assertCleanState

# ( n u -- ) \ prints a number with the given base to emitFd
#           ascii          , len, flag, label
def_callword "PNTTESTPASS" ,  11 ,  0 ,  pntTestPass

# ( -- ) \ prints the current stack
#           ascii , len, flag, label
def_callword "PNTSTACK" ,  8 ,  0 ,  pntStack

# ( -- ) \ prints current stack/register/etc info
#           ascii       , len, flag, label
def_callword "DUMPINFO" ,  8 ,  0 ,  dumpInfo

# ( addr count -- ) \ dump contents of address
#            ascii     , len, flag, label
def_callword "DUMP" ,  4 ,  0 ,  dump

# ( -- ) \ exits immediately, printing info
#            ascii     , len, flag, label
def_callword "DBGEXIT" ,  7 ,  0 ,  dbgexit

# ( sc -- eax ) \ Call a syscall with no arguments
#            ascii     , len, flag, label
def_asmword "SYSCALL0",   8,    0, syscall0
  dpop %eax
  int $LINUX_SYSCALL
  dpush %eax
  NEXT

# ( arg0 sc -- eax ) \ Call a syscall with one argument
#            ascii     , len, flag, label
def_asmword "SYSCALL1",   8,    0, syscall1
  dpop %eax
  dpop %ebx
  int $LINUX_SYSCALL
  dpush %eax
  NEXT

# ( arg1 arg0 sc -- eax ) \ Call a syscall with two arguments
#            ascii     , len, flag, label
def_asmword "SYSCALL2",   8,    0, syscall2
  dpop %eax
  dpop %ebx
  dpop %ecx
  int $LINUX_SYSCALL
  dpush %eax
  NEXT

# ( arg2 arg1 arg0 sc -- eax ) \ Call a syscall with three arguments
#            ascii     , len, flag, label
def_asmword "SYSCALL3",   8,    0, syscall3
  dpop %eax
  dpop %ebx
  dpop %ecx
  dpop %edx
  int $LINUX_SYSCALL
  dpush %eax
  NEXT

# ( a b -- a+b ) \ add values
#           ascii  , len, flag, label
def_asmword "+"    , 1  ,  0 ,  ADD
  movl $8, %eax
  call checkStackSize
  dpop %eax
  addl %eax, (%ebp)
  NEXT

# ( u -- u ) \ add 1
#            ascii  , len, flag, label
def_asmword "1+"    , 2  ,  0 ,  _1ADD
  dpop %eax
  inc %eax
  dpush %eax
  NEXT

# ( u -- u ) \ add 2
#            ascii  , len, flag, label
def_asmword "2+"    , 2  ,  0 ,  _2ADD
  dpop %eax
  addl $2, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ add 4
#            ascii  , len, flag, label
def_asmword "4+"    , 2  ,  0 ,  _4ADD
  dpop %eax
  addl $4, %eax
  dpush %eax
  NEXT

# ( a b -- a-b ) \ subtract values
#           ascii  , len, flag, label
def_asmword "-"    , 1  ,  0 ,  SUB
  dpop %eax
  subl %eax, (%ebp)
  NEXT

# ( u -- u ) \ sub 1
#            ascii  , len, flag, label
def_asmword "1-"    , 2  ,  0 ,  _1SUB
  dpop %eax
  subl $1, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ sub 2
#            ascii  , len, flag, label
def_asmword "2-"    , 2  ,  0 ,  _2SUB
  dpop %eax
  subl $2, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ sub 4
#            ascii  , len, flag, label
def_asmword "4-"    , 2  ,  0 ,  _4SUB
  dpop %eax
  subl $4, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ divide a number by two (floor)
#           ascii  , len, flag, label
def_asmword "2/" , 2  ,  0 ,  _2DIV
  dpop %eax
  shr $1, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ divide a number by four (floor)
#           ascii  , len, flag, label
def_asmword "4/" , 2  ,  0 ,  _4DIV
  dpop %eax
  shr $2, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ divide a number by sixteen (floor)
#           ascii  , len, flag, label
def_asmword "16/" , 3  ,  0 ,  _16DIV
  dpop %eax
  shr $4, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ multiply a number by 4
#           ascii  , len, flag, label
def_asmword "4*" , 2  ,  0 ,  _4MUL
  dpop %eax
  shl $2, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ multiply a number by 16
#           ascii  , len, flag, label
def_asmword "16*" , 3  ,  0 ,  _16MUL
  dpop %eax
  shl $4, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ shift bits left by 8
#          ascii  , len, flag, label
def_asmword "8SHL",   4,    0, _8shl
  dpop %eax
  shl $8, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ shift bits left by 16
#          ascii   , len, flag, label
def_asmword "16SHL",   6,    0, _16shl
  dpop %eax
  shl $16, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ shift bits right by 8
#          ascii  , len, flag, label
def_asmword "8SHR",   4,    0, _8shr
  dpop %eax
  shr $8, %eax
  dpush %eax
  NEXT

# ( u -- u ) \ shift bits right by 16
#           ascii  , len, flag, label
def_asmword "16SHR",   5,    0, _16shr
  dpop %eax
  shr $16, %eax
  dpush %eax
  NEXT

# ( u:a u:b -- u:quot u:rem ) \ divide a/b -> quotient remainder
#           ascii  , len, flag, label
def_asmword "/MOD" , 4  ,  0 ,  DIVmod
  call DIVmod
  NEXT

# ( u -- u ) \ increment by 1
#           ascii, len, flag, label
def_asmword "INC",   3,    0, inc
  movl $4, %eax
  call checkStackSize
  addl $1, (%ebp)
  NEXT

# ( u -- u ) \ increment by 4
#            ascii, len, flag, label
def_asmword "INC4",   4,    0, inc4
  movl $4, %eax
  call checkStackSize
  addl $4, (%ebp)
  NEXT

# ( u -- u ) \ increment by 1
#           ascii, len, flag, label
def_asmword "DEC",   3,    0, dec
  movl $4, %eax
  call checkStackSize
  subl $1, (%ebp)
  NEXT

# ( u -- u ) \ increment by 4
#            ascii, len, flag, label
def_asmword "DEC4",   4,    0, dec4
  movl $4, %eax
  call checkStackSize
  subl $4, (%ebp)
  NEXT

# ( u u -- u ) \ bitwise AND
#          ascii  , len, flag, label
def_asmword "AND" ,  3 ,   0 , and
  dpop %eax
  andl %eax, (%ebp)
  NEXT

# ( u u -- u ) \ bitwise OR
#          ascii , len, flag, label
def_asmword "OR" ,  2 ,   0 , or
  movl $8, %eax
  call checkStackSize
  dpop %eax
  orl %eax, (%ebp)
  NEXT

# ( u u -- u ) \ bitwise XOR (exclusive OR)
#          ascii , len, flag, label
def_asmword "XOR" ,  3 ,   0 , xor
  movl $8, %eax
  call checkStackSize
  dpop %eax
  xorl %eax, (%ebp)
  NEXT

# ( u -- u ) \ bitwise INVERT (switch all bits)
#          ascii     , len, flag, label
def_asmword "INVERT" ,  6 ,   0 , invert
  dpop %eax
  notl %eax
  dpush %eax
  NEXT

not:
  dpop %eax
  cmp $0, %eax
  je 1f
  dpush $FALSE  # false
  ret
1:
  dpush $TRUE # true
  ret

# ( u -- flag ) \ equal to 0?
#          ascii   , len, flag, label
def_callword "NOT" , 3 ,   0 , not

# ( u -- flag ) \ equal to 0?
#          ascii , len, flag, label
def_asmword "=0",  2 ,   0 , EQ0
  call not
  NEXT

bool: # ( n -- flag )  \ convert integer to TRUE or FALSE
  cmp $0, (%ebp)      # if FALSE just return
  je 1f
  movl $TRUE, (%ebp)  # else force TRUE
1:
  ret

# ( u -- flag ) \ convert integer to TRUE or FALSE
#          ascii  , len, flag, label
def_callword "BOOL",   4,   0, bool

# Macro for defining comparisons, which are identical except their jmp
# conditions.
.macro def_asmcmp name, namelen, flags, label, jumpcondition
  def_asmword "\name" , \namelen , \flags , \label
  dpop %ebx
  dpop %eax
  cmp %ebx, %eax
  \jumpcondition 1f
  dpush $FALSE  # false
  NEXT
1:
  dpush $TRUE # true
  NEXT
.endm

# ( u:a u:b -- flag:a=b ) \ equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp "="   , 1  ,   0 , EQ   , je

# ( u:a u:b -- flag:a<>b ) \ not equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp "<>"   , 2 ,   0 , NEQ  , jne

# ( u:a u:b -- flag:a<b ) \ less than
#          ascii , len, flag, label, jumpcondition
def_asmcmp "<"   , 1 ,   0 , LT  , jl

# ( u:a u:b -- flag:a<=b ) \ less than or equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp "<="  , 2 ,   0 , LTE , jle

# ( u:a u:b -- flag:a>b ) \ greater than
#          ascii , len, flag, label, jumpcondition
def_asmcmp ">"   , 1 ,   0 , GT  , jg

# ( u:a u:b -- flag:a>=b ) \ greater than or equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp ">="  , 2 ,   0 , GTE , jge


# ( -- u ) \ used to compile literal values. Simply returns the value in the next
# "instruction", skipping its execution.
#           ascii , len, flag, label
def_asmword "LIT",    3,   0,    lit
  lodsl  # %eax=(%esi) %esi+=4, see NEXT
  dpush %eax
  NEXT

# ( -- addr count ) \ used to construct strings and other literal data, similar
# \ to LIT above. The memory looks like this:
# \    xt_litbytes | len | cell0 | cell1 | cell2 | ... | cellN
#               ascii , len, flag, label
def_asmword "LITBYTES",   8,    0, litbytes
  lodsl  # %eax=(%esi) %esi+=4, see NEXT
  dpush %esi  # address
  dpush %eax  # count
  # start execution after the literal bytes (aligned)
  addl %eax, %esi
  addl $3, %esi  # see eax_align4 for explanation
  andl $~3, %esi
  NEXT

# ( -- arru8 count ) \ used to construct strings and other literal data, similar
# \ to LIT above. The memory looks like this:
# \    xt_litcarru8 | u8:count | u8 | u8 | u8 | ... | cellN
#                 ascii, len, flag, label
def_asmword "LITCARRU8",   9,    0, litcarru8
  movl $0, %eax
  movb (%esi), %al # count
  inc %esi
  dpush %esi  # arru8
  dpush %eax  # count
  addl %eax, %esi
  addl $3, %esi  # see eax_align4 for explanation
  andl $~3, %esi
  NEXT

# # ( -- arru8 count ) \ Same as litcarru8 except uses 16bit count.
# # \    xt_litcarru8 | u16:count | u8 | u8 | u8 | ... | cellN
# #                 ascii , len, flag, label
# def_asmword "LITC2ARRU8",  10,    0, litc2arru8
#   lodsl  # %eax=(%esi) %esi+=4, see NEXT
#   dpush %esi  # address
#   dpush %eax  # count
#   # start execution after the literal bytes (aligned)
#   addl %eax, %esi
#   addl $3, %esi  # see eax_align4 for explanation
#   andl $~3, %esi
#   NEXT

# ( -- addr count ) \ needed in definition of map\"
#           ascii , len, flag, label
def_asmword "_SERR",   5,    0, _STRerror
  dpush $errorUnknownEscape
  call countnt
  NEXT

# ( -- addr count ) \ needed in definition of \"
#           ascii , len, flag, label
def_asmword "_SCERR",   6,    0, _STRcounterror
  dpush $errorCountExceeded
  call countnt
  NEXT

.align 4
xt_testLitBytes:
.int DOCOL
.int xt_litbytes
.int 15 # length
testLitBytes_str:
.ascii "--testLitbytes-"  # exra added for misaligment
.align 4
# ( addr count )
testLitBytes_cont:
.int xt__2dup, xt_lit, testLitBytes_str, xt_lit, 15, xt_ceq, xt_assertTrue
.int xt_pntTestPass
.int xt_EXIT
testLitbytes:
  callXt $xt_testLitBytes
  ret

##############################
# Strings and Input / Output
# Some of these we have already defined, but we 

# ( src dst count -- ) \ Moves bytes from src to dst
#           ascii    , len, flag, label
def_callword "BMOVE" ,  5 ,  0 ,  bmove

# ( src dst count -- ) \ Moves bytes from src to dst starting at right-most bytes
#           ascii    , len, flag, label
def_asmword "BMOVE>",   6,   0 , bmoveRIGHT
  # Forth BMOVE, move count bytes from addr1 -> addr2.
  dpop %edi # count/index
  dpop %ecx # addr2
  movl %eax, %edi  # index
  cmp $0, %edi # if count=0 exit
  je 2f
  dec %edi
1:
  movb (%ebx,%edi,1), %dl
  movb %dl, (%ecx,%edi,1)
  dec %edi
  jnz 1b
2:
  ret

# ( src dst count -- ) \ Moves bytes from src to dst
#                ascii    , len, flag, label
def_callword "BMOVEUPPER" , 10 ,    0, bmoveUpper

# ( addr -- addr count ) \ counts a null-terminated (c-style) string.
#           ascii      , len, flag, label
def_callword "COUNTNT" ,  7 ,  0 ,  countnt

# ( addr -- addr count ) \ convert c8bytes (8bit counted bytes) to addr count
#           ascii  , len, flag, label
def_asmword "COUNT8",   6,   0 , count8
  dpop %eax        # addr
  movl $0, %ebx
  movb (%eax), %bl # count
  inc %eax
  dpush %eax
  dpush %ebx
  NEXT

# \ forth COMPARE, returns -1 if str1<str2, 0 iff ==, 1 otherwise
# ( addr1 count1 addr2 count2 -- i ) 
#           ascii    , len, flag, label
def_asmword "COMPARE",   7,   0 , compare
  movl 12(%ebp), %eax  # addr1
  movl 4(%ebp), %ebx  # addr2
  movl $0, %edi  # index
compare_loop:
  cmpl %edi, (%ebp)
  je compare_exhausted
  cmpl %edi, 8(%ebp)
  je compare_exhausted
  movb (%eax,%edi,1), %cl  # addr1+index
  cmpb (%ebx,%edi,1), %cl  # addr2+index
  jl compare_lt
  jg compare_gt
  inc %edi
  jmp compare_loop # equal, loop again
compare_exhausted: # count ran out
  movl (%ebp), %eax  # count2
  cmpl %eax, 8(%ebp)
  jl compare_lt
  jg compare_gt
  jmp compare_eq
compare_eq:
  movl $0, %eax
  jmp compare_done
compare_lt:
  movl $-1, %eax
  jmp compare_done
compare_gt:
  movl $1, %eax
  jmp compare_done
compare_done:
  addl $16, %ebp  # _4drop
  dpush %eax
  NEXT

# ( addr1 count1 addr2 count2 -- flag ) \ return whether bytes1==bytes2
#         ascii  , len, flag, label
def_asmword "CEQ",   3,   0 , ceq
  movl $16, %eax
  call checkStackSize
  movl (%ebp), %eax
  cmp %eax, 8(%ebp)  # cmp count1 to count2
  jne ceq_ne
  callXt $xt_compare
  call not
  NEXT
ceq_ne:
  call _4drop
  dpush $FALSE
  NEXT

# ( addr1 count1 addr2 count2 -- flag )
# \ return whether bytes1==bytes2 with case ignored
#         ascii  , len, flag, label
def_asmword "BYTESEQNOCASE", 9, 0, bytesEqNoCase
  movl $16, %eax
  call checkStackSize
  movl (%ebp), %eax
  cmp %eax, 8(%ebp)  # cmp count1 to count2
  jne byteseq_igcase_ne
  movl 4(%ebp), %ecx  # addr2
  movl 12(%ebp), %eax # addr1
  movl $0, %edi  # index
byteseq_igcase_loop:
  cmpl %edi, (%ebp)
  je byteseq_igcase_eq # reached count bytes, equal
  movb (%eax,%edi,1), %bl # move to bl, convert>upper, move to bh
  call bl_toupper
  movb %bl, %bh
  movb (%ecx,%edi,1), %bl # move to bl, convert>upper, cmp w/bh
  call bl_toupper
  cmpb %bl, %bh
  jne byteseq_igcase_ne
  inc %edi
  jmp byteseq_igcase_loop
byteseq_igcase_ne:
  call _4drop
  dpush $FALSE
  NEXT
byteseq_igcase_eq:
  call _4drop
  dpush $TRUE
  NEXT

testBytesEqNoCase:
  dpush $testCreatedWord
  call countnt
  call _2dup
  callXt $xt_bytesEqNoCase
  dpop %eax
  movl $TRUE, %ebx
  call assertEaxEbxEq

  dpush $testCreatedWord
  call countnt
  dpush $testCreatedWordUpper
  call countnt
  callXt $xt_bytesEqNoCase
  dpop %eax
  movl $TRUE, %ebx
  call assertEaxEbxEq

  dpush $testCreatedWord
  call countnt
  dpush $testCreatedWordDifferent
  call countnt
  callXt $xt_bytesEqNoCase
  dpop %eax
  movl $FALSE, %ebx
  call assertEaxEbxEq
  
  ret

# ( addr count -- addr count ) \ remove trailing spaces
#               ascii  , len, flag, label
def_asmword "-TRAILING",   9,    0, SUBtrailing
  movl 4(%ebp), %eax  # address
  movl (%ebp), %ebx   # count
  dec %ebx            # index (last index first)
SUBtrailing_loop:
  cmpl $0, %ebx       # if index<0 exit
  jl SUBtrailing_done
  cmpb $' ', (%eax,%ebx,1)  # if not equal to space, no more trailing
  jne SUBtrailing_done
  dec %ebx            # decrement index
  jmp SUBtrailing_loop
SUBtrailing_done:
  inc %ebx            # index -> count
  movl %ebx, (%ebp)   # update count
  NEXT

# ( c -- ) \ emit a byte
#           ascii   , len, flag, label
def_callword "EMIT" , 4  ,   0 , emit

# ( addr count -- ) \ print a string to emitFd
#           ascii   , len, flag, label
def_callword ".S" ,  2 ,  0 ,  pntS

# ( addr count -- ) \ pnts a string to emitFd with newline
#           ascii , len, flag, label
def_asmword ".SLN",   4,    0, pntSln
  callXt $xt_pntS
  dpush $'\n'
  call emit
  NEXT

# ( -- ) \ panics immediately with ERR_SEE_MSG
#            ascii   , len, flag, label
def_asmword "PANIC" ,  5 ,  0 ,  panic
  movl $ERR_SEE_MSG, panicErr
  call *(panic)

####
# Builtin Test
# Wow, that was a lot of words! We aren't going to test them all here (we will
# do full tests in forth), but it's worth making sure some core ones work. We
# are going to write these tests "forth-style" and execute them with callXt.
#
# As you can see, these are already easier to read and write than previous
# assembly. It's almost forth-like already!

.align
testDictBuiltinLit:
  # : <annon> 42 ;
  .int DOCOL, xt_lit, 42, xt_EXIT
testDictBuiltinAdd:
  # : <annon> 32 10 + 42 assertEq ;
  .int DOCOL, xt_lit, 32, xt_lit, 10, xt_ADD, xt_lit, 42, xt_assertEq, xt_EXIT
testDictBuiltinSUBtrailing:
  .int DOCOL, xt_lit, testWordWithSpaces, xt_countnt, xt_SUBtrailing
  .int xt_lit, 4, xt_assertEq, xt_drop, xt_EXIT
testDictBuiltinCompare:
  # COMPARE wordWithSpaces w/ self
  .int DOCOL, xt_lit, testWordWithSpaces, xt_countnt, xt__2dup
  .int xt_compare, xt_lit, 0, xt_assertEq
  # COMPARE wordWithSpaces w/ -trailing self
  .int xt_lit, testWordWithSpaces, xt_countnt, xt__2dup, xt_SUBtrailing
  .int xt_compare, xt_lit, 1, xt_assertEq
  # COMPARE abc w/ bde
  .int xt_lit, testAbcStr, xt_countnt, xt_lit, testBcdStr, xt_countnt
  .int xt_compare, xt_lit, -1, xt_assertEq
  # CEQ wordWithSpaces w/ -trailing self
  .int xt_lit, testWordWithSpaces, xt_countnt, xt__2dup, xt_SUBtrailing
  .int xt_ceq, xt_lit, FALSE, xt_assertEq
  # CEQ abc w/ bcd
  .int xt_lit, testAbcStr, xt_countnt, xt_lit, testBcdStr, xt_countnt
  .int xt_ceq, xt_lit, FALSE, xt_assertEq
  # CEQ abc w/ self
  .int xt_lit, testAbcStr, xt_countnt, xt__2dup
  .int xt_ceq, xt_lit, TRUE, xt_assertEq
  .int xt_EXIT
testDictBuiltinType:
  # : <annon> 32 10 + 42 assertEq ;
  .int DOCOL, xt_lit, testDictBuiltinStr, xt_countnt, xt_pntTestPass, xt_EXIT
testDictBuiltin:
  # The bare minimum to make sure we are actually calling "real" code.
  callXt $testDictBuiltinLit
  dpop %eax
  movl $42, %ebx
  call assertEaxEbxEq

  callXt $testDictBuiltinAdd
  callXt $testDictBuiltinSUBtrailing
  callXt $testDictBuiltinCompare
  call assertEmpty
  callXt $testDictBuiltinType
  ret

sysread: # %ebx:fd %ecx:&buff %edx:maxLength ( -- ) %eax:numRead
  # Read from a filedescriptor. This does NOT panic on failure, the caller must
  # check %eax for errors.
  movl $SYS_READ, %eax
  int $LINUX_SYSCALL
  ret

readKey: # ( -- ) \ reads a byte into keyBuffer
  movl keyFd, %ebx
  movl $keyBuffer, %ecx
  movl $1, %edx
  call sysread
  cmp $0, %eax
  jl readKeyErr
  ret
readKeyErr:
  movl %eax, panicErr
  movl $ERR_EMIT_ERR, panicErr
  call *(panic)
  jmp unreachable

#           ascii , len, flag, label
def_asmword "KEY" ,   3,    0, key
  call readKey
  cmpl $0, %eax
  je 1f # EOF reached, interpret as "0"
  movl keyBuffer, %eax
  dpush %eax
  NEXT
1:
  dpush $0
  NEXT

sysopen: # %ebx:&fnameNt %ecx:intention %edx:permissions ( -- ) %eax:fd
  # Open the nt filename.
  # check %eax for errors and panic if they exist.
  movl $SYS_OPEN, %eax
  int $LINUX_SYSCALL
  cmp $0, %eax
  jl 1f
  ret
1:
  movl %eax, panicErr
  call *(panic)
  jmp unreachable

sysclose: # %ebx:fd ( -- ) 
  # Close the filedescriptor
  movl $SYS_CLOSE, %eax
  int $LINUX_SYSCALL
  cmp $0, %eax
  jl 1f
  ret
1:
  movl %eax, panicErr
  call *(panic)
  jmp unreachable

openNtEbx:
  movl $O_RDONLY, %ecx
  movl $0666, %edx  # permissions
  call sysopen
  movl %eax, keyFd
  ret

openSimpleMathFile:
  movl $testSimpleMathFile, %ebx
  call openNtEbx
  ret

closeKeyFd:
  movl keyFd, %ebx
  call sysclose
  movl $STDIN, keyFd
  ret

testKey:
  call openSimpleMathFile
  callXt $xt_key
  dpush $'0'
  callXt $xt_assertEq

  callXt $xt_key
  dpush $'x'
  callXt $xt_assertEq

  call closeKeyFd
  dpush $testKeyStr
  call countnt
  call pntTestPass
  ret

bl_toupper: # %bl:b ( -- ) %bl:b
  # toupper: Convert ascii byte from lower->upper case.
  # input: %bl byte to convert
  # return: %bl return value
  #
  # Notes: 'A'=0x41 or 65 'Z'=0x5A or 90
  #        'a'=0x61 or 97 'z'=0x7A or 122
  # If the byte is not within ['a', 'z'] then don't change
  cmpb $'a', %bl
  jl 1f
  cmpb $'z', %bl
  jg 1f
  # else subtract 0x20 to convert to upper case.
  subb $0x20, %bl
1:
  ret

# ( addr count -- addr count ) \ Convert to upper ASCII case
#              ascii ,  len, flag, label
def_asmword "TOUPPER",    7,    0, toupper
  movl (%ebp), %ecx   # count
  movl 4(%ebp), %edx  # addr
  movl $0, %edi       # index
1:
  cmp %edi, %ecx  # compare count to index
  je 2f
  movb (%edx,%edi,1), %bl
  call bl_toupper
  movb %bl, (%edx,%edi,1)
  jmp 1b          # loop
2:
  NEXT

# ( -- addr count ) \ Get the next WORD on the input stream
#            ascii ,  len, flag, label
def_asmword "WORD",    4,    0, word
  dpush $0        # count
word_loop:
  cmpl $MAX_WORD_LEN, (%ebp)
  jg word_err
  call readKey
  cmp $0, %eax # check for EOF
  je word_eof
  movb keyBuffer, %bl
  # check for whitespace: all bytes > ascii-space are not whitespace. This
  # means utf8 is supported out of the box, since all non-ascii utf8 bytes are
  # > 127
  cmpb $' ', %bl 
  jle word_ret_maybe
  # move byte uppercased into buffer
  movl (%ebp), %eax  # count
  addl $wordBuffer, %eax
  movb %bl, (%eax)
  addl $1, (%ebp)  # increment count
  jmp word_loop
word_ret_maybe:   # possibly return a word... or keep looping if count=0
  cmp $0, (%ebp)  # are there any bytes in the buffer?
  je word_loop    # word not yet encountered, keep looping
word_eof: # (in case of EOF we still push the count)
  movl (%ebp), %eax
  movl %eax, wordCount
  dpush $wordBuffer # ( count addr )
  call swap
  movl $0x300, %edx  # TODO: remove
  NEXT
word_err:
  dpush $maxWordLenErrorStr
  call pntSNt
  movl $ERR_MAX_WORD_LEN, panicErr
  call *(panic)

testWord:
  call openSimpleMathFile
  callXt $xt_word
  call testWord_assert0x

  callXt $xt_word # "12"
  dpush $2
  callXt $xt_assertEq # len
  callXt $xt_dup
  callXt $xt_bFETCH
  dpush $'1'
  callXt $xt_assertEq
  addl $1, (%ebp) # add 1 to addr since count=2
  callXt $xt_bFETCH
  dpush $'2'
  callXt $xt_assertEq

  callXt $xt_word
  call testWord_assert0x
  call closeKeyFd
  dpush $testWordStr
  call countnt
  call pntTestPass
  ret
testWord_assert0x: # ( addr count )
  dpush $2
  callXt $xt_assertEq # count
  callXt $xt_dup
  callXt $xt_bFETCH
  dpush $'0'
  callXt $xt_assertEq # 1
  addl $1, (%ebp) # add 1 to addr since count=2
  callXt $xt_bFETCH
  dpush $'x'
  callXt $xt_assertEq # 2
  ret

ebx_asciitou: # %ebx ( -- ) %ebx \ convert from a ascii to an integer
  cmpb $'0', %bl # Check if < 0 (invalid)
  jl ebx_asciitou_err
  cmpb $'9', %bl # Check if <= 9 (valid 0-9)
  jle ebx_asciitou_0to9
  call bl_toupper
  cmpb $'A', %bl # Check if ['A', 'Z']
  jl ebx_asciitou_err
  cmpb $'Z', %bl
  jg ebx_asciitou_err
  subb $'A', %bl  # convert A->10, B->11, etc
  addb $10, %bl
  ret
ebx_asciitou_0to9:
  subb $'0', %bl
  ret
ebx_asciitou_err:
  addl $1, %ebx
  neg %ebx
  ret

# ( --  ) \ Line comment using "\"
#          ascii, len, flag, label
def_asmword "\\",   1, FR_TIME_IMM, linecomment
  call readKey
  cmp $0, %eax # check for EOF
  je 1f
  cmpb $'\n', keyBuffer
  jne word_linecomment # loop
1:
  NEXT

# ( -- c ) \ Convert the single-byte "word" to it's ASCII representation
#   \ panics if the next word has len > 1
#            ascii, len, flag, label
def_asmword "ASCII",   5,    0, ascii
  callXt $xt_word # ( addr count )
  dpop %eax
  cmp $1, %eax  # see if length <> 1
  jne 1f
  dpop %eax  # get the address
  movl $0, %ebx  # zero ebx and move ascii into bl
  movb (%eax), %bl
  dpush %ebx     # push byte on stack
  NEXT
1:
  movl $ERR_INVALID_CHAR, panicErr
  call *(panic)

# ( -- u ) \ Convert the next hex word to an unsigned number, compiling it if state!=0
#   \ panics if the next word is not completely representable by hex
#          ascii, len,        flag, label
def_asmword "0X",   2, FR_TIME_IMM, _0x
  callXt $xt_word # ( addr count )
  call strTOhex
  cmp $0, var_REFstate
  jne 1f
  NEXT # runmode: leave value on stack
1: # Compile mode: compile the literal into the word
  dpush $xt_lit
  callXt $xt_pushDict
  callXt $xt_pushDict
  NEXT

strTOhex: # ( addr count -- u )
  movl $8, %eax
  call checkStackSize
  cmp $0, (%ebp)
  je _0x_noword       # word returned a count of 0=no word so panic
  movl $0, %edi       # index
  movl $0, %eax       # cumulative value
  movl $0, %ebx       # byte value
  movl 4(%ebp), %ecx  # address of word
_0x_loop:
  cmp %edi, (%ebp)  # cmp count with index
  je _0x_done
  movb (%ecx,%edi,1), %bl # get the ascii and convert to int
  call ebx_asciitou
  cmpl $0, %ebx  # check that it is between [0,15]
  jl _0x_notnumber
  cmpl $15, %ebx
  jg _0x_notnumber
  shl $4, %eax   # eax=eax*16. Note: bit-shift left 4 == *2^4= *16
  addl %ebx, %eax # add new number to cumulative
  inc %edi        # increment index
  jmp _0x_loop
_0x_done:
  movl %eax, 4(%ebp)  # turn addr -> number
  call drop  # drop count
  ret
_0x_noword:
  dpush $_0xWordNeededStr
  call countnt
  callXt $xt_pntSln
  movl $ERR_WORD_MISSING, panicErr
  call *(panic)
_0x_notnumber:
  dpush $_0xNotNumberStr
  call pntSNt
  # addr count still contains the word, simply use that
  call _2dup
  callXt $xt_pntSln
  movl $ERR_NUMBER_INVALID, panicErr
  call *(panic)

.align 4
xt_test0x:
.int DOCOL
.int xt_word, xt__2drop  # ignore "0x"
.int xt__0x # reads word and converts to number
.int xt_lit, 0x12, xt_assertEq
.int xt_word, xt__2drop  # ignore "0x"
.int xt__0x
.int xt_lit, 0x03, xt_assertEq
.int xt_lit, test0xStr, xt_countnt, xt_pntTestPass
.int xt_EXIT
test0x:
  dpush $0x3
  dpush $test3Str
  call test0x_str

  dpush $0x3C
  dpush $test3CStr
  call test0x_str

  dpush $0x42
  dpush $test42Str
  call test0x_str

  call openSimpleMathFile
  callXt $xt_test0x
  call closeKeyFd
  call assertCleanState
  ret
test0x_str: # ( expected strnt -- )
  call countnt
  call strTOhex
  dpop %eax
  dpop %ebx
  call assertEaxEbxEq
  ret

##############################
# Builtin Variables
# All of these simply return the memory location of the variables so values can
# be fetched and stored.
#
# The built-in variables are:
# &STATE      Pointer to cell, 0=runmode else=compiling
# &LATEST     Pointer to nt of the latest (most recently defined) word
# &HERE       Pointer to address of next free byte of memory.
# &RSTACK_MAX The maximum memory address of the return stack.
#
# Unlike traditional FORTH I have put & in front of these to adhere to the standard:
# These are ADDRESSES. They MUST be dereferenced '@' to read their value. They
# can also be written to with '!'.

# ( -- u )
.macro def_var name, namelen, flags, label, initial
def_asmword \name , \namelen , \flags , \label
  dpush $var_\label
  NEXT
  .data
  .align 4
var_\label :
  .int \initial
.endm

#      ascii         , len, flag,  label , initial
def_var "&STATE"     ,   6,    0, REFstate  , 0
def_var "&HERE"      ,   5,    0, REFhere   , 0
def_var "&LATEST"    ,   7,    0, REFlatest , 0 # initialized in _start
def_var "&RSTACK_MAX",  10,    0, REFrstack_max , 0 # initialized in _start

##############################
# Builtin Constants
# These simply return a constant value when run
.macro def_const name, namelen, label
def_asmword \name , \namelen , 0, \label
  dpush $\label
  NEXT
.endm

#         ascii  ,              len, label
def_const "0",                    1, 0
def_const "TRUE" ,                4, TRUE
def_const "FALSE" ,               5, FALSE
def_const "RMAX" ,                4, rstackMax
def_const "DOCOL",                5, DOCOL
def_const "F0_NAMELEN",          10, F0_NAMELEN_MASK
def_const "FR_TIME_MASK",        12, FR_TIME_MASK
def_const "FR_TIME_RUN",         11, FR_TIME_RUN
def_const "FR_TIME_IMM",         11, FR_TIME_IMM
def_const "FR_TIME_IMM_PART",    16, FR_TIME_IMM_PART
def_const "FR_TIME_RUN_PART",    16, FR_TIME_RUN_PART
def_const "ERR_SEE_MSG",         11, ERR_SEE_MSG
def_const "SYS_OPEN",             8, SYS_OPEN
def_const "SYS_WRITE",            9, SYS_WRITE
def_const "SYS_READ",             8, SYS_READ
def_const "SYS_CLOSE",            9, SYS_CLOSE
def_const "SYS_EXIT",             8, SYS_EXIT
def_const "IOB",                  3, iob

testConst:
  callXt $xt_FR_TIME_IMM
  dpop %eax
  movl $FR_TIME_IMM, %ebx
  call assertEaxEbxEq
  # Not printing test name for such a minor test
  ret

##############################
# Compiler/Interpreter Internals
# A basic compiler:
# 1. Gets words from the input stream using WORD
# 2. Looks them up in the dictionary using FIND
# 3. Gets their xt and inserts it into the current compiling word, updating
#    HERE as it goes.
#
# The "currently compiling word" is defined using CREATE, which creates a new
# dictionary entry for the next word in the input stream and sets LATEST and
# HERE to point to the appropriate places. Items are insert using the `,` or
# `C,` operators.
#
# How does code update the dictionary entry? Any word flagged as FR_TIME_MASK ==
# FR_TIME_IMM or FR_TIME_IMM_RUN gets executed immediately instead of being
# stored in the data section. Alternatively, if F0_STATE_COMPILE is set in the global
# STATE then all words (immediate or not) will be executed instead of
# compiled.

# ( nt -- addr count ) \ given the nt return the name string
#   Note that this implementation is very clever with the flags0 cell. The
#   left-most (msb) flag0 byte MUST have the 5 namelen bits in the lsb position. If
#   the namelen is 0 (which it is for `:noname` words) then all our logic will
#   work "as is." If namelen=0, the addr returned will be invalid but the count
#   will be zero!
#              ascii, len, flag, label
def_asmword "NT>STR",   6,    0, ntTOstr
  dpop %eax            # address
  addl $4, %eax        # address of name as c8bytes
  movb (%eax), %bl     # get either c8bytes count or left-most F0 byte
  andl $F0_NAMELEN_BYTE_MASK, %ebx  # clear other flags
  addl $1, %eax        # addr=addr+1 (next byte)
  dpush %eax
  dpush %ebx           # push count
  NEXT

eax_align4: # %eax ( -- ) %eax \ modify eax so it is aligned by 4
  addl $3, %eax   # Add 3 to the number, pushing it up to or over 4byte boundary
                  # iff not aligned
  andl $~3, %eax  # Same as %4 (but faster). Gets rid of two lowest bytes.
  ret

.align 4
testAlign4_0:
.byte 0
testAlign4_1:
.byte 0
testAlign4_2:
.byte 0
testAlign4_3:
.byte 0
testAlign4_4:
.byte 0
testAlign4_5:
.byte 0
.align 4
xt_testAlign4:
.int DOCOL, xt_lit, testAlign4Str, xt_countnt, xt_pntTestPass, xt_EXIT
testAlign4:
  movl $testAlign4_0, %eax
  call eax_align4
  movl $testAlign4_0, %ebx
  call assertEaxEbxEq

  movl $testAlign4_1, %eax
  call eax_align4
  movl $testAlign4_4, %ebx
  call assertEaxEbxEq

  movl $testAlign4_2, %eax
  call eax_align4
  movl $testAlign4_4, %ebx
  call assertEaxEbxEq

  movl $testAlign4_4, %eax
  call eax_align4
  movl $testAlign4_4, %ebx
  call assertEaxEbxEq

  movl $testAlign4_5, %eax
  call eax_align4
  movl $xt_testAlign4, %ebx
  call assertEaxEbxEq

  callXt $xt_testAlign4
  ret

# ( addr -- addr ) \ align on 4 byte boundary
#           ascii  , len, flag, label
def_asmword "ALIGN4",  6,    0, align4
  dpop %eax
  call eax_align4
  dpush %eax
  NEXT

# ( nt -- F0 ) \ given the nt return the address of F0
#            ascii  , len, flag, label
def_asmword "NT>&F0",   6,    0, ntTOf0
  dpop %eax           # address of nt
  cmp $0, %eax
  je 1f
  addl $4, %eax       # start of c8bytes (count)
  movb (%eax), %bl    # ebx=count
  andl $F0_NAMELEN_BYTE_MASK, %ebx
  # TODO: this actually DOESN'T work with F0 strategy.
  #   need to deal with that pesky $1
  addl %ebx, %eax     # eax=eax+count+1
  addl $1, %eax
  call eax_align4     # align, as F0 is always aligned
  dpush %eax
  NEXT
1:
  movl $ERR_NULL_NAMETOKEN, panicErr
  call *(panic)

# ( nt -- FR ) \ given the nt return the address of FR
#            ascii  , len, flag, label
def_asmword "NT>&FR",   6,    0, ntTOfr
  callXt $xt_ntTOf0
  addl $4, (%ebp)
  NEXT


# ( nt -- xt ) \ given the nt return the xt
#           ascii  , len, flag, label
def_asmword "NT>XT",   5,    0, ntTOxt
  callXt $xt_ntTOfr
  addl $4, (%ebp) # xt is directly after right-most flag (FR)
  NEXT

# ( nt -- addr count ) \ given the nt return the name
#             ascii  , len, flag, label
def_asmword "NT>NAME",   9,    0, ntTOname
  addl $4, (%ebp) # name is directly after nt
  callXt $xt_count8
  NEXT

# ( nt -- xt ) \ get "code field address" (xt) from nt
#         ascii  , len, flag, label
def_doword ">CFA",   4,    0, TOcfa
  .int xt_ntTOxt
  .int xt_EXIT

# ( nt -- addr ) \ get "data field address" from nt
#          ascii  , len, flag, label
def_doword ">DFA",   4,    0, TOdfa
  .int xt_ntTOxt
  .int xt_inc4
  .int xt_EXIT

# ( xt -- FR ) \ get FR flags of the word
#           ascii  , len, flag, label
def_doword "XT>&FR",   5,    0, xtTOfr
  .int xt_dec4
  .int xt_EXIT


.align 4
xt_test_ntTOother_constructed0: # unaligned name
.int 1      # fake link
.byte 2     # fake count
.byte 1,2   # fake string
.align 4
.int 0x42     # fake F0
.int 0x43     # fake FR
.int 0x44     # fake xt
xt_test_ntTOother_constructed1: # aligned name
.int 1      # fake link
.byte 3     # fake count
.byte 1,2,3 # fake string
.align 4
.int 0x42     # fake F0
.int 0x43     # fake FR
.int 0x44     # fake xt
.align 4
xt_test_ntTOother:
.int DOCOL
flags:
# already aligned, c8bytes=4bytes
.int xt_lit, nt_lit, xt_ntTOf0, xt_lit, flags_lit, xt_assertEq
# flags: not aligned, c8bytes=5bytes
.int xt_lit, nt_word, xt_ntTOf0, xt_lit, flags_word, xt_assertEq
# nt>xt: already aligned
.int xt_lit, nt_lit, xt_ntTOxt, xt_lit, xt_lit, xt_assertEq
# # nt>xt: not aligned
.int xt_lit, nt_word, xt_ntTOxt, xt_lit, xt_word, xt_assertEq
# Test this because I've hit issues
.int xt_lit, nt_SEMICOLON, xt_ntTOfr, xt_FETCH, xt_lit, FR_TIME_IMM, xt_assertEq
.int xt_assertEmpty, xt_lit, test_ntTOotherStr, xt_countnt, xt_pntTestPass
.int xt_EXIT
test_ntTOother:
  # demonstration of dump
  dpush $xt_test_ntTOother_constructed0
  dpush $20
  call dump

  dpush $xt_test_ntTOother_constructed0
  call test_ntTOother_constructed
  dpush $xt_test_ntTOother_constructed1
  call test_ntTOother_constructed
  callXt $xt_test_ntTOother
  ret
.align 4
test_ntTOother_constructed: # ( nt -- )
  # Test a constructed dict entry which always contains
  # certain data (but not the same name)
  call dup
  callXt $xt_ntTOf0
  dpop %eax
  movl (%eax), %eax
  movl $0x42, %ebx
  call assertEaxEbxEq

  call dup
  callXt $xt_ntTOfr
  dpop %eax
  movl (%eax), %eax
  movl $0x43, %ebx
  call assertEaxEbxEq

  call dup
  callXt $xt_ntTOxt
  dpop %eax
  movl (%eax), %eax
  movl $0x44, %ebx
  call assertEaxEbxEq

  call dup
  callXt $xt_ntTOxt
  callXt $xt_xtTOfr
  dpop %eax
  movl (%eax), %eax
  movl $0x43, %ebx
  call assertEaxEbxEq

  call drop
  ret

# ( addr count -- nt ) \ finds the string in the dictionary, returning it's nt (name token)
#                      \ or 0 if not found
#           ascii  , len, flag, label
def_asmword "FIND" , 4 ,    0 , find
  movl $8, %eax
  call checkStackSize
  movl var_REFlatest, %eax
  dpush %eax    # points to the nt of word we are looking at
  call rrot     # Stack: ( nt addr count )
find_loop:
  call _2dup # ( nt addrf countf addrf countf )
  movl 16(%ebp), %ebx  # nt
  dpush %ebx # ( nt addrf countf addrf countf nt )
  callXt $xt_ntTOstr   # ( nt -- addr count )

  callXt $xt_bytesEqNoCase  # find if strings are equal
  dpop %eax                 # if equal, return nt
  cmp $TRUE, %eax           # stack: ( nt addrf countf )
  je find_found
  jmp find_next
find_found:
  call _2drop # drop ( addr count ) leaving nt
  NEXT
find_next:
  movl 8(%ebp), %ebx  # see if next link is 0, exit if is
  movl (%ebx), %eax   # %eax contains next nt
  cmp $0, %eax
  je find_not_found
  movl %eax, 8(%ebp)  # point link to next item
  jmp find_loop
find_not_found:
  call _3drop
  dpush $0
  NEXT


.align 4
xt_testFind:
.int DOCOL
# Cannot find word which DNE
.int xt_lit, testAbcStr, xt_countnt, xt_find, xt_lit, 0, xt_assertEq
# Find SWAP
.int xt_lit, c8bytes_swap, xt_count8, xt_find, xt_lit, nt_swap, xt_assertEq
# Find EXIT
.int xt_lit, c8bytes_EXIT, xt_count8, xt_find, xt_lit, nt_EXIT, xt_assertEq
# Find 0x
.int xt_lit, c8bytes__0x, xt_count8, xt_find, xt_lit, nt__0x, xt_assertEq
.int xt_lit, testFindStr, xt_countnt, xt_pntTestPass
.int xt_EXIT
.align 4
testFind:
  callXt $xt_testFind
  ret

DOCREATE: # ( -- xt-body ) \ the (default) execution word for CREATE. Simply
      # returns the >BODY (where HERE pointed to after CREATE) of the xt being
      # executed. This allows it to be used for defining variables.
  addl $4, %eax  # eax holds current xt (from NEXT)
  dpush %eax     # push the body
  NEXT

# ( addr namelen -- nt ) \ create the next word on the input stream in the dictionary
#             ascii  , len, flag, label
def_asmword "CREATE" ,  6 ,    0 , create
  movl $8, %eax
  call checkStackSize
  cmp $MAX_WORD_LEN, (%ebp) # check if namelen > max
  jg create_fail
  movl var_REFhere, %edi  # edi=here=nt of this word
  movl var_REFlatest, %eax   # latest=previous word
  movl %eax, (%edi)       # dict: link to previous
  movl %edi, var_REFlatest   # dict: update LATEST to this word
  dpush %edi              # add nt onto the stack (to return)
  call rrot               # ( nt addr namelen )
  addl $4, %edi           # go to where c8bytes is stored
  movl (%ebp), %ecx       # ecx=namelen
  movb %cl, (%edi)        # dict: namelen
  inc %edi
  call tuck   # ( nt namelen addr namelen ) note: doesn't touch edi
  movl %edi, var_REFhere # store edi in HERE, as bmove will destroy
  dpush %edi  # addr2
  call swap           # stack: ( nt namelen addr addr2 namelen )
  call bmoveUpper     # dict: put name string
  dpop %ecx         # namelen. stack: ( nt )
  movl var_REFhere, %eax   # restore HERE into eax
  addl %ecx, %eax       # add namelen to HERE and align
  call eax_align4
  shl $F0_NAMELEN_SHIFT, %ecx # shift namelen to use as F0
  movl %ecx, (%eax)           # dict: set F0
  addl $4, %eax               # increment eax to point to FR
  movl $0, (%eax)             # set FR to 0
  addl $4, %eax               # increment eax to point to xt
  movl $DOCREATE, (%eax)      # dict: xt for CREATE-ed word
  addl $4, %eax               # increment eax to point to data space
  movl %eax, var_REFhere         # dict: update HERE
  NEXT                        # return ( nt )
create_fail:
  movl $ERR_MAX_WORD_LEN, panicErr
  call *(panic)

.align 4
xt_testCreate:
.int DOCOL
.int xt_REFhere, xt_FETCH, xt_rpush
.int xt_lit, testCreatedWord, xt_countnt, xt_create # ( nt )
.int xt_dup, xt_assertTrue # assert there is a name token
# Assert HERE has changed
.int xt_rpop, xt_REFhere, xt_FETCH, xt_NEQ, xt_assertTrue
# Assert xt of latest = here - 4
.int xt_dup, xt_ntTOxt, xt_REFhere, xt_FETCH, xt_dec4, xt_assertEq
# Find the word and assert nt is the same
.int xt_dup, xt_lit, testCreatedWordUpper, xt_countnt, xt_find, xt_assertEq
# stack: ( nt )
# Flags are as expected (namelen)
.int xt_dup, xt_ntTOf0, xt_FETCH, xt_lit, 11<<F0_NAMELEN_SHIFT, xt_assertEq
# Stored name is uppercased
.int xt_ntTOname, xt_lit, testCreatedWordUpper, xt_countnt
.int xt_ceq, xt_assertTrue
.int xt_lit, testCreateStr, xt_countnt, xt_pntTestPass
.int xt_EXIT
testCreate:
  callXt $xt_testCreate
  call assertCleanState
  ret

# ( u -- ) \ push a value onto the dictionary
#      ascii  , len, flag, label
def_asmword ",",  1,    0, pushDict
  dpop %eax
  movl var_REFhere, %ebx
  movl %eax, (%ebx)  # move value into *here
  addl $4, var_REFhere  # increment here for next value
  NEXT

# ( -- ) \ Set compilation STATE to false
#        ascii  , len,       flag, label
def_asmword "[" ,  1 , FR_TIME_IMM, staterun
  cmpl $FALSE, var_REFstate
  je 1f
  movl $FALSE, var_REFstate
  NEXT
1:
  movl $ERR_STATE_REDUNDANT, panicErr
  call *(panic)

# ( -- ) \ Set compilation STATE to true
#        ascii  , len, flag, label
def_asmword "]" ,  1 ,    0, statecompile
  cmpl $TRUE, var_REFstate
  je 1f
  movl $TRUE, var_REFstate
  NEXT
1:
  movl $ERR_STATE_REDUNDANT, panicErr
  call *(panic)

# ( -- ) \ Used by ':' or anyone to create a word with xt=DOCOL
#               ascii  , len, flag, label
def_doword "CREATEWORD",  10,    0, createWord
  .int xt_word # ( addr count )
  .int xt_create, xt_ntTOxt # ( xt )
  # replace DOCREATE with DOCOL
  .int xt_lit, DOCOL, xt_swap # ( DOCOL xt )
  .int xt_STORE # compile DOCOl into xt
  .int xt_EXIT

# ( -- ) \ require the state to be in run mode
#                ascii  , len, flag, label
def_asmword "REQUIRERUN",  10,    0, requireRun
  cmp $0, var_REFstate
  jne 1f
  NEXT
1:
  movl $ERR_STATE_INCORRECT, panicErr
  call *(panic)

# ( -- ) \ require the state to be in compile mode
#                 ascii  , len, flag, label
def_asmword "REQUIRECOMP",  11,    0, requireComp
  cmp $0, var_REFstate
  je 1f
  NEXT
1:
  movl $ERR_STATE_INCORRECT, panicErr
  call *(panic)

# ( -- ) \ The forth ":" to define a new word
#      ascii  , len, flag, label
def_doword ":",   1,    0, COLON
  # Create the next word and set correct xt
  .int xt_requireRun
  .int xt_createWord
  .int xt_statecompile # start compiling!
  .int xt_EXIT

# ( -- ) \ require the stack to be empty or panic
#                  ascii  , len, flag, label
def_asmword "REQUIREEMPTY",  12,    0, requireEmpty
  call depth
  dpop %eax
  cmp $0, %eax
  jne 1f
  NEXT
1:
  movl $ERR_STACK_NOT_EMPTY, panicErr
  call *(panic)

# ( -- ) \ end compilation from ":"
#      ascii  , len,        flag, label
def_doword ";",   1, FR_TIME_IMM, SEMICOLON
  .int xt_requireComp
  .int xt_requireEmpty
  .int xt_lit, xt_EXIT, xt_pushDict  # compile EXIT
  .int xt_staterun # [ go back to runmode
  .int xt_EXIT

.align 4
xt_testFlags:
.int DOCOL
# F0
.int xt_lit, flags_SEMICOLON, xt_FETCH, xt_lit, 1 << F0_NAMELEN_SHIFT
.int   xt_assertEq
# FR
.int xt_lit, flags_SEMICOLON, xt_inc4, xt_FETCH, xt_lit, FR_TIME_IMM
.int   xt_assertEq
.int xt_lit, testFlagsStr, xt_countnt, xt_pntTestPass
.int xt_EXIT
testFlags:
  callXt $xt_testFlags
  ret

.align 4
xt_testCOLON:
.int DOCOL
.int xt_REFhere, xt_FETCH, xt_rpush
# Make sure state=run
.int xt_REFstate, xt_FETCH, xt_lit, FALSE, xt_assertEq
# start compilation, mimicking INTERPRET
.int xt_word, xt_find, xt_ntTOxt, xt_execute  # \
.int xt_word, xt_find, xt_ntTOxt, xt_execute  # :
# Make sure state=compile
.int xt_REFstate, xt_FETCH, xt_lit, TRUE, xt_assertEq
# manually finish compilation, mimicking INTERPRET
.int xt_word, xt_find, xt_ntTOxt, xt_execute  # 0x
.int  xt_word, xt_find, xt_ntTOxt, xt_execute # ;
# assert state is back in runmode
.int xt_REFstate, xt_FETCH, xt_lit, FALSE, xt_assertEq
#### Validate the word cell by cell
.int xt_lit, testNewDefinedWordStr, xt_countnt, xt_find
# Make sure previous HERE = nt
.int xt_dup, xt_rpop, xt_assertEq
# Make sure the nt strings match
.int xt_dup, xt_ntTOstr, xt_lit, testNewDefinedWordStr, xt_countnt
.int   xt_ceq, xt_assertTrue
# Make sure str len is as expected in c8bytes
.int xt_dup, xt_inc4, xt_bFETCH, xt_lit, 14, xt_assertEq
# Make sure str len is as expected in F0
.int xt_dup, xt_ntTOf0, xt_inc4, xt_dec, xt_bFETCH, xt_lit, 14, xt_assertEq
# Make sure FR
.int xt_dup, xt_ntTOfr, xt_FETCH, xt_lit, 0, xt_assertEq
# Make sure xt
.int xt_dup, xt_ntTOxt, xt_FETCH, xt_lit, DOCOL, xt_assertEq
# Validate our manual compilation
.int xt_ntTOxt, xt_dec4, xt_dec4 # get F0
.int xt_dup, xt_FETCH, xt_lit, 14<<F0_NAMELEN_SHIFT, xt_assertEq
.int xt_inc4 # get FR
.int xt_dup, xt_FETCH, xt_lit, 0, xt_assertEq
.int xt_inc4, xt_inc4 # back to xt -> first data
.int xt_dup, xt_FETCH, xt_lit, xt_lit, xt_assertEq
.int xt_inc4  # 0x42
.int xt_dup, xt_FETCH, xt_lit, 0x42, xt_assertEq
.int xt_inc4  # EXIT
.int xt_dup, xt_FETCH, xt_lit, xt_EXIT, xt_assertEq
.int xt_drop # drop address
# Now run the word, mimicking the interpreter
.int xt_word, xt_find, xt_ntTOxt, xt_execute  # newDefinedWord
.int xt_lit, 0x42, xt_assertEq
# drop addr, set state back to run and exit
.int xt_EXIT
xt_testCOLON_pnt:
.int DOCOL, xt_lit, testCOLONStr, xt_countnt, xt_pntTestPass, xt_EXIT
testCOLON:
  movl $testDefineFile, %ebx
  call openNtEbx
  callXt $xt_testCOLON
  call closeKeyFd
  callXt $xt_testCOLON_pnt
  ret

# ( -- ) \ tag currently compiling word as TIME_IMM(ediate)
# \ This is traditionally called IMMEDIATE.
#          ascii, len, flag, label
def_doword "IMM",   3, FR_TIME_IMM, imm
  .int xt_REFlatest, xt_FETCH, xt_ntTOfr # ( &FR )
  # Clear TIME flags
  .int xt_dup, xt_dup, xt_FETCH # ( &FR &FR FR )
  .int xt_FR_TIME_MASK, xt_invert, xt_and # ( &FR &FR (FR AND FR_TIME_MASK) )
  .int xt_swap, xt_STORE
  # Store TIME_IMM
  .int xt_dup, xt_FETCH # ( &FR FR )
  .int xt_FR_TIME_IMM, xt_or, xt_swap, xt_STORE
  .int xt_EXIT

# ( -- xt ) \ Instead of executing the next xt, put it on the stack.
#         ascii, len, flag, label
def_asmword "'",   1,    0, xt
  lodsl  # %eax=(%esi) %esi+=4 -- see NEXT
  dpush %eax  # but push xt on stack instead of jmp *(...)
  NEXT

# ( -- ) \ add offset (next cell) to the forth instruction pointer
#   \ performing an unconditional jump.
#              ascii, len, flag, label
def_asmword "BRANCH",   6,    0, branch
  addl (%esi), %esi
  NEXT

# ( flag -- ) \ perform BRANCH iff flag=0
#               ascii, len, flag, label
def_asmword "0BRANCH",   7,    0, _0branch
  # NOTE: %eax contains xt of current instruction from NEXT. Don't override it!
  dpop %ebx  # count
  cmp $0, %ebx
  je word_branch  # if 0, jump to code in BRANCH above
  lodsl           # otherwise skip the offset
  NEXT

# ( flag -- ) \ perform BRANCH iff the first byte=0. Don't
# \ consume first word. This will be used for error checking.
#                 ascii, len, flag, label
def_asmword "?BRANCH",   7,    0, QUESTIONbranch
  # NOTE: %eax contains xt of current instruction from NEXT. Don't override it!
  cmpb $0, (%ebp)
  je word_branch  # if 0, jump to code in BRANCH above
  lodsl           # otherwise skip the offset
  NEXT

# ( xt -- ??? ) \ Execute the given xt
#               ascii, len, flag, label
def_asmword "EXECUTE",   7,    0, execute
  # NOTE: %eax contains xt of current instruction from NEXT. Don't override it!
  dpop %eax  # xt
  call _callXt # expects %eax=xt
  NEXT

# ( xt -- ??? ) \ exec or compile the given xt given STATE and the xt's TIME_IMM
#                     ascii, len, flag, label
def_callword "EXEC|COMPILE",  12,    0, execORcompile
execORcompile:
  movl $4, %eax
  call checkStackSize
  # Check if state = 0 (executing)
  cmpl $0, var_REFstate
  je eorc_exec
  call dup
  callXt $xt_xtTOfr # ( xt &FR )
  dpop %ebx         # ( xt ) eax=&FR
  movl (%ebx), %eax
  andl $FR_TIME_MASK, %eax
  cmpl $FR_TIME_IMM, %eax
  je eorc_exec
  cmpl $FR_TIME_IMM_PART, %eax
  je eorc_err_badflag  # not supported by this interpreter
  cmpl $FR_TIME_RUN_PART, %eax
  je eorc_err_badflag  # not supported by this interpreter
  # Not executing and not immediate: compile word
  callXt $xt_pushDict
  ret
eorc_exec:
  dpop %eax
  call _callXt
  ret
eorc_err_badflag:
  movl $ERR_INVALID_FLAG, panicErr
  call *(panic)

# ( -- ) \ Interpret bytes on the input stream until EOF
# \ Basic design:
# \ - call WORD
# \ - call FIND
# \ - execute word (callXt)
# \ - repeat until EOF
# \ Note: we will create a more user-friendly interpreter in forth.
# \ this is good enough for bootstrapping us.
#                 ascii, len, flag, label
def_asmword "INTERPRET",   9,    0, interpret
interpret_loop:
  callXt $xt_word
  cmp $0, (%ebp) # if count == 0, exit
  je interpret_next
  callXt $xt_find
  cmp $0, (%ebp) # if nt is 0 then error
  je interpret_err_noword
  callXt $xt_ntTOxt
  call execORcompile
  jmp interpret_loop
interpret_next:
  call _2drop
  NEXT
interpret_err_noword:
  movl $ERR_WORD_NOT_FOUND, panicErr
  call *(panic)

.align 4
xt_testInterpret_theAnswer:
.int DOCOL
.int xt_interpret, xt_lit, 0x42, xt_assertEq
.int xt_EXIT
xt_testInterpret_inputstream:
.int DOCOL, xt_interpret, xt_assertEmpty, xt_EXIT
xt_testInterpret_pnt:
.int DOCOL, xt_lit, testInterpretStr, xt_countnt, xt_pntTestPass, xt_EXIT
testInterpret:
  dpush $testTheAnswerFile
  call pntSNtln
  movl $testTheAnswerFile, %ebx
  call openNtEbx
  callXt $xt_testInterpret_theAnswer
  call closeKeyFd

  dpush $testSimpleMathFile
  call pntSNtln
  call openSimpleMathFile
  callXt $xt_testInterpret_inputstream
  call closeKeyFd

  dpush $triforthFile
  call pntSNtln
  movl $triforthFile, %ebx
  call openNtEbx
  callXt $xt_testInterpret_inputstream
  call closeKeyFd

  callXt $xt_testInterpret_pnt
  ret

# !!! NOTE: this must be the last word (or update var_REFlatest) !!!
# ( -- ) \ QUIT is the forth interpreter loop
#            ascii, len, flag, label
def_asmword "QUIT",   4,    0, quit
  # set the return stack to rstackMax (clear it)
  .int xt_REFrstack_max, xt_FETCH, xt_rstackSTORE # clear the rstack
  .int xt_interpret    # interpreter loop
  .int xt_branch, -8   # loop interpret forever

##############################
# Main Entrypoint
.globl _start
_start:
  movl $heapMin, var_REFhere
  movl $nt_quit, var_REFlatest
  movl $rstackMax, var_REFrstack_max
  movl $rstackMax, %esp  # clear rstack
  movl $dstackMax, %ebp  # clear dstack
  call testSuite

  # Reset everything to get rid of words/etc defined during test.
  movl $heapMin, var_REFhere
  movl $nt_quit, var_REFlatest
  movl $rstackMax, %esp  # clear rstack
  movl $dstackMax, %ebp  # clear dstack
  jmp exit0

.data
.align 128
# The dictionary grows starting here
heapMin: .space 131072  # 128 KiB
heapMax:
