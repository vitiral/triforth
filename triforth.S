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
# The first reason is because programming languages are built on too much complexity
# today. So much complexity that most students cannot imagine how it all started. How
# DID people program the first C compilers? Modern C compilers are written in C and
# are so complicated (with their complex preprocessor and the amount of standards they
# must adhere to) that they require a binary to bootstrap them, a binary which
# is completely obtuse and immune to understanding from naive students of
# computers like ourselves.
#
# Understanding doesn't have to be so HARD. The forth compiler has a peculiar ability
# to walk VERY early, after only a few lines of assembly.  In this tutorial we
# will use x86 assembly to bootstrap our own forth language, called triforth
# which is a pun from the "triforce" from the legend of Zelda. Like the
# triforce, triforth will give us the Power to write scalable type-safe
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
#           %eip: inst ptr
#           %eflags: we won't deal with this
#
# TODO: add addressing modes and critical instructions
#
# Token Types: these are the token types. See "Forth Dictionary" for more details
# nt: name token, the POINTER to the first item in the dictionary (link)
# xt: execution token, the POINTER to the item in a dictionary which itself
#     points to the code to run

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

# Our Panic Error Codes
.equ ERR_PSTACK_OVERFLOW,0x300
.equ ERR_PSTACK_UNDERFLOW,0x301
.equ ERR_ASCII_BOUNDS,0x302
.equ ERR_MAX_WORD_LEN,0x303
.equ ERR_WORD_MISSING,0x304
.equ ERR_NUMBER_INVALID,0x305

.equ FALSE, 0
.equ TRUE, -1

####
# Flags
# We will get into these details when we are constructing the type system.
# The only flags you have to pay attention to for now are:
# NAMELEN: allows backreference from xt->nt
# TIME_MASK and related: specifies when the xt should be run, and whether it
#   has both IMM and RUN components (i.e. : word IMMEDIATE ... DOES> ... EXIT)

## Flag 0 (left-most flag)
# Byte 1 (least-significant right-most byte)
.equ F0_RES,               0x000000FF  # 8b: reserved
# Byte 2
.equ F0_INP_MASK,          0x00000700  # 3b: input size mask
.equ F0_OUT_MASK,          0x00003800  # 3b: output size mask
.equ F0_TIME_MASK,         0x0000C000  # 2b: time mask (run/compile/etc)
.equ F0_TIME_RUN,          0x00000000  # na: runtime only word
.equ F0_TIME_IMM,          0x00004000  # na: immediate only word
.equ F0_TIME_IMM_PART,     0x00008000  # na: immediate part of immediate+run
.equ F0_TIME_RUN_PART,     0x0000C000  # na: run part of immediate+run
# Byte 3
.equ F0_IMM_SIZE,          0x00FF0000  # 8b: immediate code size
# Byte 4 (most-significant left-most byte)
.equ F0_NAMELEN_BYTE_MASK, 0x0000001F  # na: mask used for left-most byte
.equ F0_NAMELEN_MASK,      0x1F000000  # 5b: name's length (up to 32)
.equ F0_NAMELEN_SHIFT,     24          # na: number of bits to shift
.equ F0_TYPE_MASK,         0xE0000000  # 3b: the type (8 types)
.equ F0_TYPE_UNTYPED,      0x00000000  # na: untyped word
.equ F0_TYPE_TYWORD,       0x20000000  # na: typed word
.equ F0_TYPE_STRUCT,       0x40000000  # na: struct (data structure)
.equ F0_TYPE_METHOD,       0x60000000  # na: method (attached typed word_
.equ F0_TYPE_INTERFACE,    0x80000000  # na: interface (collection of methods)
.equ F0_TYPE_MODULE,       0xA0000000  # na: module (module-name and file-path)
.equ F0_TYPE_RES1,         0xC0000000  # na: type reserved
.equ F0_TYPE_RES2,         0xE0000000  # na: type reserved

## Flag1 TODO: implement this for module system
.equ F1_MODULE_ID,        0x000000FF  # 8b: module id this word is from
.equ F1_RES,              0x0000FF00  # 8b: reserved
.equ F1_LINENO,           0xFFFF0000  # 16b: line number


######################################################
### DATA SECTION
######################################################
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
ioBuffer: .space 128
ioBufferLen: .int 0  # number of chars in ioBuffer
.equ MAX_WORD_LEN,32
wordBuffer: .space MAX_WORD_LEN
state: .int 0
.equ FSTATE_COMPILE, 0x01  # 0=run 1=compile
.equ NOT_FSTATE_COMPILE, ~FSTATE_COMPILE

# Word info
.equ FLAG_WORD_IMMEDIATE, 0x01 # whether the word is immediate or not


# Address that should be called when panicing, can be redirected when testing.
panic: .int defaultPanic
# Specefic error code encountered when panicing.
panicErr: .int 0
# Used in `reset` to return from a panic in testing.
rstackCheckpoint: .int 0

emitFd: .int STDERR # where to EMIT and TYPE to
.equ EMIT_BUFFER_SIZE,16
emitBuffer: .space EMIT_BUFFER_SIZE # scratch space for emit and DOT
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
ioBufferStr: .ascii "ioBuffer: \0"
stackStr: .ascii "Stack< \0"
maxWordLenErrorStr: .ascii "Maximum word length exceeded\0"
_0xWordNeededStr: .ascii "Word needed for 0x\0"
_0xNotNumberStr: .ascii "Not a valid hex number: \0"


# Test Names
testIoBufferStr: .ascii "testIoBuffer\0"
testDOTStr: .ascii "testDOT\0"
testDOTsStr: .ascii "testDOTs\0"
testDumpInfoStr: .ascii "testDumpInfo\0"
testNEXTStr: .ascii "testNEXT\0"
testEXITStr: .ascii "testEXIT\0"
testDictSimpleStr: .ascii "testDictSimple\0"
testDictBuiltinStr: .ascii "testDictBuiltin\0"
testKeyStr: .ascii "testKey\0"
testWordStr: .ascii "testWord\0"
testNumberStr: .ascii "testNumber\0"
testAlign4Str: .ascii "testAlign4\0"
test_ntTOotherStr: .ascii "test_ntTOother\0"
testFindStr: .ascii "testFind\0"
testCreateStr: .ascii "testCreate\0"

# Test Files
testSimpleMathFile: .ascii "tests/simplemath.fs\0"

# Test Inputs
testWordWithSpaces: .ascii " spc    \0"
testAbcStr: .ascii "abc\0"
testBcdStr: .ascii "bcd\0"
testCreatedWord: .ascii "createdword\0"
testCreatedWordUpper: .ascii "CREATEDWORD\0"

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
  call testMemLocations
  call assertEmpty
  call testAsserts
  call testIndirectCall
  call testPanicHandle
  call testDsPushPop
  call testModDIV
  call testDepth
  call testStackBasic
  call assertCleanState

  call testEmit
  call testCountNt
  call testIoBuffer
  call testDOT
  call testDOTs
  call testMUL
  call testDumpInfo
  call assertCleanState

  call testNEXT
  call testEXIT
  call testDictSimple
  call assertCleanState

  call testDictBuiltin
  call testKey
  call testWord
  call assertCleanState

  call testNumber
  call testAlign4
  call test_ntTOother
  call testFind
  call testCreate
  call assertCleanState
  ret

testMemLocations:
  # Sanity check to make sure memory locations are set as expected.
  movl $rstackMax, %eax
  subl $rstackMin, %eax
  cmp $RSTACK_SIZE, %eax
  jne 1f
  movl $ioBufferLen, %eax
  subl $IO_BUFFER_SIZE, %eax
  cmp $ioBuffer, %eax
  jne 1f
  ret
1:
  call testFailed

testFailed: # The test failed
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
  jne testFailed
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
  call testFailed
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
  movl $ERR_PSTACK_OVERFLOW, panicErr
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
  movl $ERR_PSTACK_UNDERFLOW, panicErr
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
  call testFailed
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

DIVmod: # ( u:a u:b -- u u )
  # Forth MOD/, unsigned divide a by b
  # returns: quotient remainder
  dpop %ebx # divisor
  movl $0, %edx
  dpop %eax # %edx:%eax=number

  divl %ebx  # %edx:%eax / %ebx => %eax=quotient %edx=remainder
  dpush %eax
  dpush %edx
  ret

testModDIV:
  dpush $13
  dpush $5
  call DIVmod
  dpop %ebx
  movl $3, %eax # remainder
  call assertEaxEbxEq
  dpop %eax
  movl $2, %ebx # quotient
  call assertEaxEbxEq
  call assertEmpty
  ret

swap: # ( a b -- b a )
  # Forth SWAP
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %eax
  ret

dup: # ( a -- a a )
  movl (%ebp), %eax
  dpush %eax
  ret

_2dup: # ( a b -- a b a b )
  movl 4(%ebp), %eax
  movl (%ebp), %ebx
  dpush %eax
  dpush %ebx
  ret

drop: # ( a -- )
  addl $4, %ebp
  ret

_2drop: # ( a b -- )
  addl $8, %ebp
  ret

_3drop: # ( a b c -- )
  addl $12, %ebp
  ret

_4drop: # ( a b c d -- )
  addl $16, %ebp
  ret

lrot: # ( a b c -- b c a )
  # Forth ROT, rotates left
  dpop %ecx
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %ecx
  dpush %eax
  ret

rrot: # ( a b c -- c a b )
  # Forth -ROT, rotates right
  dpop %ecx
  dpop %ebx
  dpop %eax
  dpush %ecx
  dpush %eax
  dpush %ebx
  ret

over: # ( a b -- a b a )
  movl 4(%ebp), %eax
  dpush %eax
  ret

depth: # ( -- u )
  # returns the depth of the stack
  movl $dstackMax, %eax
  subl %ebp, %eax
  dpush %eax
  dpush $4
  call DIVmod
  call drop # drop remainder
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
# return code, including DOTs (.S) which allows us to view the stack at any
# time.
# In addition, many of these will eventually be included directly as forth
# words, so by defining them as early as possible we can get the greatest use
# of them.

emit: # ( c -- )
  # emits a single character to emitFd
  dpop %eax
  movb %al, emitBuffer
  movl emitFd, %ebx   # fd
  movl $emitBuffer, %ecx # &buf
  movl $1, %edx       # bytesToWrite
syswrite: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
  # Direct call to SYS_WRITE syscall with error checking.
  movl $SYS_WRITE, %eax
  int $0x80
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

type: # ( addr count -- )
  # Forth TYPE. Writes count characters at addr to emitFd
  movl emitFd, %ebx   # fd
  dpop %edx # bytesToWrite=count
  dpop %ecx # &buf
  call syswrite
  ret

typeTestPass: # ( addr count -- )
  # types "TEST <addr count> PASS\n" to emitFd
  dpush $testPrefixStr
  call countnt
  call type
  call type
  dpush $testPassStr
  call countnt
  call type
  ret

testIoBuffer:
  dpush $testIoBufferStr
  call countnt
  call typeTestPass
  ret

cSTORE: # ( c addr -- )
  # forth C!. Store character at address
  dpop %eax  # addr
  dpop %ebx  # c
  movb %bl, (%eax)
  ret

DOT: # ( n -- )
  # forth '.', emit the value on the stack.
  movl var_base, %eax  # note: will be defined later
  dpush %eax
  call DOTbase
  dpush $' '
  call emit
  ret

DOTbase: # ( n u:base -- )
  # emit the number using the given base.
  # TODO: deal with negatives
  dpush $0 # count: characters in buffer
  call rrot # ( count n base )
1:
  call dup  # ( count n base base )
  call rrot # ( count base n base )
  call DIVmod # ( count base quot rem )
  call uTOascii # ( count base quot c )

  # write characters right->left
  movl $emitBufferMax-1, %eax
  subl 12(%ebp), %eax # note: count at index=3
  dpush %eax # address to store character at
  # ( count base quot c addr )
  call cSTORE # ( count base quot )
  call swap   # ( count quot base )
  addl $1, 8(%ebp) # increment count
  cmp $0, 4(%ebp) # check if quot=0
  jne 1b # loop

  call _2drop # no need for quot or base
  movl $emitBufferMax, %eax
  subl (%ebp), %eax
  dpush %eax # ( count addr )
  call swap
  call type
  dpush $' '
  call emit
  ret

uTOascii: # ( u -- c )
  # convert a number (typically 1-16) into ascii equivalent
  # This is used with DOTbase (above)
  dpop %ebx
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
  dpush %ebx
  ret
3: # error
  movl $ERR_ASCII_BOUNDS, panicErr
  call *(panic)
  call unreachable

testDOT:
  dpush $1
  call DOT
  call assertEmpty
  dpush $12
  call DOT
  dpush $0x1A2B
  dpush $16
  call DOTbase
  dpush $'\n'
  call emit
  dpush $testDOTStr
  call countnt
  call typeTestPass
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

DOTs: # ( -- )
  # Forth .S, prints the current stack
  dpush $stackStr
  # "Stack< DEPTH > "
  call countnt
  call type

  call depth 
  dpush $10
  call DOTbase

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
  dpush $16
  call DOTbase

  addl $1, (%ebp) # increment index
  jmp 1b # loop

2:
  call _2drop
  dpush $'\n'
  call emit
  ret

testDOTs:
  dpush $0x01
  dpush $0x02
  dpush $0xAA
  dpush $0x1A2B
  call DOTs
  call _2drop
  call _2drop
  dpush $testDOTsStr
  call countnt
  call typeTestPass
  ret


cmove: # ( addr1 addr2 count -- )
  # Forth CMOVE, move count characters from addr1 -> addr2.
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

cmoveUpper: # ( addr1 addr2 count -- )
  # Move count chars from addr1 -> addr2 as uppercase
  dpop %ecx # count
  dpop %edx # addr2
  dpop %ebx # addr1
  movl $0, %edi  # index
1:
  cmp %edi, %ecx
  je 2f
  movb (%ebx,%edi,1), %al
  call al_toupper
  movb %al, (%edx,%edi,1)
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
  call countnt
  call type
  movl $registerSnapshot, %ebx
  movl (%ebx), %eax  # eax
  dpush %eax
  dpush $16
  call DOTbase

  movl $registerSnapshot, %ebx
  movl 4(%ebx), %eax # ebx
  dpush %eax
  dpush $16
  call DOTbase

  movl $registerSnapshot, %ebx
  movl 8(%ebx), %eax # ecx
  dpush %eax
  dpush $16
  call DOTbase

  movl $registerSnapshot, %ebx
  movl 12(%ebx), %eax # edx
  dpush %eax
  dpush $16
  call DOTbase

  movl $registerSnapshot, %ebx
  dpush $registersDsStr
  call countnt
  call type
  movl $registerSnapshot, %ebx
  movl 16(%ebx), %eax # edi
  dpush %eax
  dpush $16
  call DOTbase

  movl $registerSnapshot, %ebx
  movl 20(%ebx), %eax # esi
  dpush %eax
  dpush $16
  call DOTbase
  dpush $'\n'
  call emit

  # Stack
  call DOTs
  # IO buffer
  dpush $ioBufferStr
  call countnt
  call type
  dpush $ioBuffer
  movl ioBufferLen, %eax
  dpush %eax
  call type
  dpush $'\n'
  call emit

  call restoreRegisters
  ret

testDumpInfo:
  # preload ioBuffer with something
  dpush $testDumpInfoStr
  call countnt
  movl (%ebp), %eax
  movl %eax, ioBufferLen
  dpush $ioBuffer
  call swap
  call cmove

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
  movl $0, ioBufferLen
  dpush $testDumpInfoStr
  call countnt
  call typeTestPass
  ret

dbgexit: # ( -- )
  # Exits immediately with rc=99
  # Mainly used in debugging.
  call dumpInfo
  dpush $dbgexitStr
  call countnt
  call type
  movl $RC_DBG, %ebx
  jmp sysexit

defaultPanic: # ( -- )
  # TODO: grow the data stack with emergency reservers
  # The default panic implemention uses the above tools to ouput
  # relevant panic messages.
  call dumpInfo
  dpush $panicStr
  call countnt
  call type
  movl panicErr, %eax
  call DOT
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
  lodsl 
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
  # In this test we will store our fake "xt's" in the ioBuffer, since we know
  # it isn't being used.
  movl $ioBuffer, %esi
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
  call typeTestPass
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
  movl $ioBuffer, %ebx

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

  # movl $ioBuffer+16, %eax  # QUAD xt
  callXt $ioBuffer+16

  dpop %eax
  movl $32, %ebx
  call assertEaxEbxEq

  dpush $testEXITStr
  call countnt
  call typeTestPass
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
cstr_EXIT:
  .byte 4       # name length
  .ascii "EXIT" # the word name
  .align 4
flags_EXIT:
  .int (4<<F0_NAMELEN_SHIFT)  # flags (just the NAMELEN)
xt_EXIT:
  .int word_EXIT # xt

# ( a b -- a*b ) \ multiply
nt_MUL:
  .int LINK            # link to previous (in this case 0)
  .set LINK, nt_MUL  # set link for next word to be defined
cstr_MUL:
  .byte 3       # name length
  .ascii "MUL"  # the word name
  .align 4
flags_MUL:
  .int (3<<F0_NAMELEN_SHIFT)  # flags (just the NAMELEN)
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
  movl $ioBuffer, %ebx
  movl $DOCOL, (%ebx)
  movl $xt_MUL, 4(%ebx)
  movl $xt_MUL, 8(%ebx)
  movl $xt_EXIT, 12(%ebx)

  # movl $ioBuffer, %eax
  callXt $ioBuffer

  dpop %eax
  movl $42, %ebx
  call assertEaxEbxEq
  dpush $testDictSimpleStr
  call countnt
  call typeTestPass
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
cstr_\label :
  .byte \namelen       # the length of the name
  .ascii "\name"       # the ascii name itself
  .align 4             # padding to next 4 byte boundary
flags_\label :
  .int (\namelen<<F0_NAMELEN_SHIFT) + \flags          # flags for the word
xt_\label :
  .int word_\label
word_\label :
.endm

# Since we have already written so many of the funcitons we need, we will write
# a second macro. All it does is call the function we have already written.

.macro def_callword name, namelen, flags=0, label
def_asmword \name , \namelen , \flags , \label
  call \label
  NEXT
.endm

# Finally, this macro allows for defining a word in terms of calling other
# words.
# TODO: write test to ensure these are equivalent
.macro def_doword name , namelen , flags=0 , label
  .align 4
nt_\label :
  .int LINK             # link to previous
  .set LINK,nt_\label   # update link to self
cstr_\label :
  .byte \namelen        # the length of the name
  .ascii "\name"        # the ascii name itself
  .align 4              # padding to next 4 byte boundary
flags_\label :
  .int \flags           # flags for the word
xt_\label :
  .int DOCOL
word_\label :           # follow with .int for xt's to call
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

# ( a -- )
#           ascii ,  len, flag, label
def_callword "DROP" , 4 ,  0 ,   drop

# ( a b -- )
#           ascii ,  len, flag, label
def_callword "2DROP" , 5 ,  0 ,   _2drop

# ( a b c -- b c a ) \ rotates left
#           ascii ,  len, flag, label
def_callword "ROT" , 3 ,  0 ,   lrot

# ( a b c -- c a b ) \ rotates right
#           ascii  , len, flag, label
def_callword "-ROT" , 4 ,  0 ,   rrot

# ( a b -- a b a )
#           ascii  , len, flag, label
def_callword "OVER" , 4 ,  0 ,   over

# ( a b -- b ) \ drops item below top of stack
#           ascii , len, flag, label
def_asmword "NIP" , 3 ,  0   , nip
  dpop %ebx
  dpop %eax
  dpush %ebx
  NEXT

tuck:
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %eax
  dpush %ebx
  ret

# ( a b -- b a b ) \ copy the top stack item below the second stack item
#            ascii  , len, flag, label
def_callword "TUCK" , 4 ,  0 ,   tuck

# ( a -- a a? ) \ duplicate top of stack if non-zero
#           ascii  , len, flag, label
def_asmword "?DUP" , 4 ,  0 ,   QDUP
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
  dpush %ebp
  RET

# ( addr -- * ) \ set the data stack pointer
#         ascii   , len, flag, label
def_asmword "DSP!",   4,    0, dstackSTORE
  movl (%ebp), %ebp
  RET

# ( -- u ) \ depth of the stack
#           ascii   , len, flag, label
def_callword "DEPTH" , 5 ,  0 ,   depth

# ( u addr -- ) \ stores the value at address
#         ascii , len, flag, label
def_asmword "!" ,  1 ,  0 ,  STORE
  dpop %eax # addr
  dpop %ebx # u
  movl %ebx, (%eax)
  NEXT

# ( addr -- u ) \ fetches the value at address
#         ascii , len, flag, label
def_asmword "@" ,  1 ,  0 ,  FETCH
  movl (%ebp), %eax
  movl (%eax), %eax
  movl %eax, (%ebp)
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

# ( c addr -- ) \ stores a character at address
#           ascii , len, flag, label
def_callword "C!" ,  2 ,  0 ,  cSTORE

# ( addr -- c ) \ fetches a character at address
#           ascii , len, flag, label
def_asmword "C@" ,  2 ,  0 ,  cFETCH
  dpop %eax # addr
  movl $0, %ebx
  movb (%eax), %bl
  dpush %ebx
  NEXT

# ( n -- ) \ prints a number to emitFd
#           ascii, len, flag, label
def_callword "." ,  1 ,  0 ,  DOT

# ( -- ) \ asserts that stack is empty
#                    ascii,len, flag, label
def_callword "ASSERTEMPTY", 11,  0, assertEmpty

# ( flag -- ) \ true or fail test
#           ascii,        len, flag, label
def_asmword "ASSERTTRUE",  10,    0, assertTrue
  dpop %eax
  cmp $0, %eax
  je 1f
  NEXT
1:
  call testFailed
  call unreachable

# ( flag -- ) \ false or fail test
#           ascii,         len, flag, label
def_asmword "ASSERTFALSE",  11,    0, assertFalse
  dpop %eax
  cmp $0, %eax
  jne 1f
  NEXT
1:
  call testFailed
  call unreachable

# ( u u -- ) \ equal or fail test
#           ascii,     len, flag, label
def_asmword "ASSERTEQ",  8,    0, assertEq
  dpop %eax
  dpop %ebx
  call assertEaxEbxEq
  NEXT

# ( n u -- ) \ prints a number with the given base to emitFd
#            ascii          , len, flag, label
def_callword "typeTestPass" ,  12 ,  0 ,  typeTestPass

# ( n u -- ) \ prints a number with the given base to emitFd
#           ascii      , len, flag, label
def_callword ".BASE" ,  5 ,  0 ,  DOTbase

# ( -- ) \ prints the current stack
#           ascii , len, flag, label
def_callword ".S" ,  2 ,  0 ,  DOTs

# ( -- ) \ prints current stack/register/etc info
#           ascii       , len, flag, label
def_callword "DUMPINFO" ,  8 ,  0 ,  dumpInfo

# ( -- ) \ exits immediately, printing info
#            ascii     , len, flag, label
def_callword "DBGEXIT" ,  7 ,  0 ,  dbgexit

# ( a b -- a+b ) \ add values
#           ascii  , len, flag, label
def_asmword "+"    , 1  ,  0 ,  ADD
  dpop %eax
  addl %eax, (%ebp)
  NEXT

# ( u -- u ) \ add 1
#            ascii  , len, flag, label
def_asmword "+1"    , 2  ,  0 ,  ADD1
  addl $1, (%ebp)
  NEXT

# ( u -- u ) \ add 4
#            ascii  , len, flag, label
def_asmword "+4"    , 2  ,  0 ,  ADD4
  addl $4, (%ebp)
  NEXT

# ( a b -- a-b ) \ subtract values
#           ascii  , len, flag, label
def_asmword "-"    , 1  ,  0 ,  SUB
  dpop %eax
  subl %eax, (%ebp)
  NEXT

# ( u -- u ) \ sub 1
#            ascii  , len, flag, label
def_asmword "-1"    , 2  ,  0 ,  SUB1
  subl $1, (%ebp)
  NEXT

# ( u -- u ) \ sub 4
#            ascii  , len, flag, label
def_asmword "-4"    , 2  ,  0 ,  SUB4
  subl $4, (%ebp)
  NEXT

# ( u:a u:b -- u u ) \ divide a/b -> quotient remainder
#           ascii  , len, flag, label
def_asmword "/MOD" , 4  ,  0 ,  DIVmod
  call DIVmod
  NEXT

# ( u -- u ) \ increment by 1
#           ascii, len, flag, label
def_asmword "INC",   3,    0, inc
  addl $1, (%ebp)
  NEXT

# ( u -- u ) \ increment by 4
#            ascii, len, flag, label
def_asmword "INC4",   4,    0, inc4
  addl $4, (%ebp)
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
  dpop %eax
  orl %eax, (%ebp)
  NEXT

# ( u u -- u ) \ bitwise XOR (exclusive OR)
#          ascii , len, flag, label
def_asmword "XOR" ,  3 ,   0 , xor
  dpop %eax
  xorl %eax, (%ebp)
  NEXT

# ( u -- u ) \ bitwise INVERT (switch all bits)
#          ascii     , len, flag, label
def_asmword "INVERT" ,  6 ,   0 , invert
  notl (%ebp)
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
  dpop %eax
  dpop %ebx
  cmp %ebx, %eax
  \jumpcondition 1f
  dpush $FALSE  # false
  NEXT
1:
  dpush $TRUE # true
  NEXT
.endm

# ( u u -- flag ) \ equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp "="   , 1  ,   0 , EQ   , je

# ( u u -- flag ) \ not equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp "<>"   , 2 ,   0 , NEQ  , jne

# ( u u -- flag ) \ less than
#          ascii , len, flag, label, jumpcondition
def_asmcmp "<"   , 1 ,   0 , LT  , jl

# ( u u -- flag ) \ less than or equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp "<="  , 2 ,   0 , LTE , jle

# ( u u -- flag ) \ greater than
#          ascii , len, flag, label, jumpcondition
def_asmcmp ">"   , 1 ,   0 , GT  , jg

# ( u u -- flag ) \ greater than or equal
#          ascii , len, flag, label, jumpcondition
def_asmcmp ">="  , 2 ,   0 , GTE , jge


# ( -- u ) \ used to compile literal values. Simply returns the value in the next
# "instruction", skipping its execution.
#           ascii , len, flag, label
def_asmword "LIT",    3,   0,    lit
  lodsl # see NEXT for what this does.
  dpush %eax
  NEXT

##############################
# Strings and Input / Output
# Some of these we have already defined, but we 

# ( src dst count -- ) \ Moves characters from src to dst
#           ascii    , len, flag, label
def_callword "CMOVE" ,  5 ,  0 ,  cmove

# ( src dst count -- ) \ Moves characters from src to dst
#                ascii    , len, flag, label
def_callword "CMOVEUPPER" , 10 ,    0, cmoveUpper

# ( addr -- addr count ) \ counts a null-terminated (c-style) string.
#           ascii      , len, flag, label
def_callword "COUNTNT" ,  7 ,  0 ,  countnt

# ( addr -- addr count ) \ convert cstr (counted str) to addr count
#           ascii  , len, flag, label
def_asmword "COUNT",   5,   0 , count
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

# ( c -- ) \ emit a character
#           ascii   , len, flag, label
def_callword "EMIT" , 4  ,   0 , emit

# ( addr count -- ) \ types a string to emitFd
#           ascii   , len, flag, label
def_callword "TYPE" ,  4 ,  0 ,  type

# ( addr count -- ) \ types a string to emitFd with newline
#            ascii   , len, flag, label
def_asmword "TYPELN" ,  6 ,  0 ,  typeln
  callXt $xt_type
  dpush $'\n'
  call emit
  NEXT

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
  .int DOCOL, xt_lit, testDictBuiltinStr, xt_countnt, xt_typeTestPass, xt_EXIT
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
  int $0x80
  ret

readKey: # ( -- ) \ reads a character into keyBuffer
  movl keyFd, %ebx
  movl $keyBuffer, %ecx
  movl $1, %edx
  call sysread
  cmp $0, %eax
  jl readKeyErr
  ret
readKeyErr:
  movl %eax, panicErr
  call *(panic)
  jmp unreachable

#           ascii , len, flag, label
def_asmword "KEY" ,   3,    0, key
  call readKey
  movl keyBuffer, %eax
  dpush %eax
  NEXT

sysopen: # %ebx:&fnameNt %ecx:intention %edx:permissions ( -- ) %eax:fd
  # Open the nt filename.
  # check %eax for errors and panic if they exist.
  movl $SYS_OPEN, %eax
  int $0x80
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
  int $0x80
  cmp $0, %eax
  jl 1f
  ret
1:
  movl %eax, panicErr
  call *(panic)
  jmp unreachable

openSimpleMathFile:
  movl $testSimpleMathFile, %ebx
  movl $O_RDONLY, %ecx
  movl $0666, %edx  # permissions
  call sysopen
  movl %eax, keyFd
  ret

closeKeyFd:
  movl keyFd, %ebx
  call sysclose
  movl $STDIN, keyFd
  ret

testKey:
  call openSimpleMathFile
  callXt $xt_key
  dpush $'1'
  callXt $xt_assertEq

  callXt $xt_key
  dpush $'2'
  callXt $xt_assertEq

  call closeKeyFd
  dpush $testKeyStr
  call countnt
  call typeTestPass
  ret

al_toupper: # %al:c ( -- ) %al:c
  # toupper: Convert character from lower->upper case.
  # input: %bl character to convert
  # return: %bl return value
  #
  # Notes: 'A'=0x41 or 65 'Z'=0x5A or 90
  #        'a'=0x61 or 97 'z'=0x7A or 122
  # If the byte is not within ['a', 'z'] then don't change
  cmpb $'a', %al
  jl 1f
  cmpb $'z', %al
  jg 1f
  # else subtract 0x20 to convert to upper case.
  subb $0x20, %al
1:
  ret

# ( addr count -- addr count ) \ Convert to upper ASCII case
#              ascii ,  len, flag, label
def_asmword "TOUPPER",    7,    0, toupper
  movl $0, %ebx
  movl (%ebp), %ecx   # count
  movl 4(%ebp), %edx  # addr
  movl $0, %edi       # index
1:
  cmp %edi, %ecx
  je 2f
  movb (%edx,%edi,1), %al
  call al_toupper
  movb %al, (%edx,%edi,1)
  jmp 1b
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
  movb keyBuffer, %al
  cmpb $'\\', %al  # comment?
  jne word_not_comment_line
word_comment_loop:
  # if comment, read till \n
  # then return with word if there is one,
  # else continue
  call readKey
  cmpb $'\n', keyBuffer
  je word_ret_maybe
  jmp word_comment_loop
word_not_comment_line:
  # check for whitespace: all characters > space are not whitespace. This means
  # utf8 is supported out of the box, since all non-ascii utf8 bytes are > 127
  cmpb $' ', %al 
  jle word_ret_maybe
  # move character uppercased into buffer
  call al_toupper
  movl (%ebp), %ebx  # count
  # movb %al, wordBuffer(,%ebx,1) # indexed addressing mode
  addl $wordBuffer, %ebx
  movb %al, (%ebx)
  addl $1, (%ebp)  # increment index
  jmp word_loop
word_ret_maybe:   # possibly return a word... or keep looping if count=0
  cmp $0, (%ebp)  # are there any characters in the buffer?
  je word_loop    # word not yet encountered, keep looping
  dpush $wordBuffer # ( count addr )
  call swap
  NEXT
word_eof:
  # EOF reached, push count even if it is 0.
  dpush $wordBuffer # ( count addr )
  call swap
  NEXT
word_err:
  dpush $maxWordLenErrorStr
  call countnt
  dpush $ioBuffer
  call swap
  call cmove
  movl $ERR_MAX_WORD_LEN, panicErr
  call *(panic)

testWord:
  call openSimpleMathFile
  callXt $xt_word
  dpush $2
  callXt $xt_assertEq # len
  callXt $xt_dup
  callXt $xt_cFETCH
  dpush $'1'
  callXt $xt_assertEq # 1

  addl $1, (%ebp) # add 1 to addr since count=2
  callXt $xt_cFETCH
  dpush $'2'
  callXt $xt_assertEq # 2

  callXt $xt_word
  dpush $1
  callXt $xt_assertEq # len
  callXt $xt_cFETCH
  dpush $'3'
  callXt $xt_assertEq # len

  call closeKeyFd
  dpush $testWordStr
  call countnt
  call typeTestPass
  ret


eax_ctou: # %eax ( -- ) %eax \ convert from a character to an integer
  cmpb $'0', %al # Check if < 0 (invalid)
  jl eax_ctou
  cmpb $'9', %al # Check if <= 9 (valid 0-9)
  jle eax_ctou_0to9
  call al_toupper
  cmpb $'A', %al # Check if ['A', 'Z']
  jl eax_ctou
  cmpb $'Z', %al
  jg eax_ctou_err
  subb $'A', %al
  ret
eax_ctou_0to9:
  subb $'0', %al
  ret
eax_ctou_err:
  addl $1, %eax
  neg %eax
  ret


# ( addr count -- n u ) \ Convert the string into a number and the remaining characters
#           ascii   ,  len, flag, label
def_asmword "NUMBER",    6,    0, number
  cmpb $0, (%ebp)
  jne 1f # count is zero, we will just push 0 in this case
  call _2drop
  dpush $0
  dpush $0
1:
  dpush $0
  call rrot           # ( isNegative addr count )
  movl $0, %edi       # index
  movl 4(%ebp), %ebx  # address
  movl $0, 4(%ebp)    # zero address, use as return number
  cmpb $'-', (%ebx)
  jne 1f
  movl $1, 8(%ebp)    # is negative=true
  inc %edi
1:
  movl $0, %eax
number_loop:
  cmp %edi, (%ebp)
  je number_done
  movb (%ebx,%edi,1), %al  # get next character
  call eax_ctou
  cmpl $0, %eax      # check for negative values
  jle number_done 
  movl %eax, %ecx    # store number we just read in ecx
  movl 4(%ebp), %eax # get cumulative
  mull var_base      # %eax=%edx:%eax*base: multiple cumulative by base
  addl %ecx, %eax    # add number we read to cumulative
  movl %eax, 4(%ebp) # store new cumulative
  inc %edi           # increment index
  jmp number_loop
number_done:
  # Note: n already stored in 4(%ebp)
  subl %edi, (%ebp)  # subtract index from count
  call lrot
  dpop %ebx # isNegative
  cmpl $0, %ebx
  jne number_isNegative # isNegative
  NEXT
number_isNegative:
  negl 4(%ebp)
  NEXT

# ( -- n ) \ Convert the next word to unsigned hex
#   \ panics if the next word is not completely representable by hex
#          ascii, len, flag, label
def_asmword "0X",   2,    0, _0x
  callXt xt_word # ( addr count )
  cmp $0, (%ebp)
  jne _0x_noword  # word returned a count of 0=no word so panic
  movl $0, %edi   # index
  movl 4(%ebp), %ebx  # address of word
  movl $0, %ecx   # cumulative value
  movl $0, %eax   # character value
  movl $0, %edx   # for mul cmd
  movl $16, -4(%ebp) # base value
1:
  cmp %edi, (%ebp)  # cmp count with index
  je _0x_done
  movb (%ebx,%edi,1), %al # get the char and convert to int
  call eax_ctou
  cmpl $0, %eax  # check that it is between [0,15]
  jl _0x_notnumber
  cmpl $15, %eax
  jg _0x_notnumber
  mull -4(%ebp)  # %edx:%eax=%edx:%eax*16: multiple cumulative by base


_0x_done:
  # TODO: implement
  call dbgexit
_0x_noword:
  dpush $_0xWordNeededStr
  call countnt
  callXt xt_typeln
  movl $ERR_WORD_MISSING, panicErr
  call *(panic)
_0x_notnumber:
  dpush $_0xNotNumberStr
  call countnt
  call type
  # addr count still contains the word, simply use that
  call _2dup
  callXt xt_typeln
  movl $ERR_NUMBER_INVALID, panicErr
  call *(panic)

.align 4
xt_testNumber:
.int DOCOL, xt_word, xt_number, xt_lit, 0, xt_assertEq  # count
.int xt_lit, 12, xt_assertEq  # number
.int xt_word, xt_number, xt_lit, 0, xt_assertEq, xt_lit, 3, xt_assertEq
.int xt_EXIT
testNumber:
  call openSimpleMathFile
  callXt $xt_testNumber

  call closeKeyFd
  dpush $testNumberStr
  call countnt
  call typeTestPass
  ret

##############################
# Builtin Variables
# All of these simply return the memory location of the variables so values can be
# fetched and stored.
#
# The built-in variables are:
# BASE       The current base for printing and reading numbers.
# STATE      Is the interpreter executing code (0) or compiling a word (non-zero)?
# LATEST     Points to the latest (most recently defined) word in the dictionary.
# HERE       Points to the next free byte of memory.  When compiling, compiled words go here.
# RSTACK_MAX The maximum memory address of the return stack.

# ( -- addr ) \ the "base" used in str<->int conversions
#      ascii      , len, flag,  label, initial
def_asmword "BASE",   4,    0,  base
  dpush $var_base
  NEXT

.macro def_var name, namelen, flags, label, initial
def_asmword \name , \namelen , \flags , \label
  dpush $var_\label
  NEXT
  .data
  .align 4
var_\label :
  .int \initial
.endm

#      ascii      ,  len, flag,  label , initial
def_var "STATE"   ,    5,    0, state  , 0
def_var "HERE"    ,    4,    0, here   , 0
def_var "LATEST"  ,    6,    0, latest , 0 # initialized in _start
def_var "RSTACK_MAX", 10,    0, rstack_max , 0 # initialized in _start

##############################
# Builtin Constants
# These simply return a constant value when run
.macro def_const name, namelen, label
  def_asmword \name , \namelen , 0, \label
  dpush $\label
  NEXT
.endm

#         ascii  ,              len, label
def_const "RMAX" ,                4, rstackMax
def_const "DOCOL",                5, DOCOL
def_const "F0_NAMELEN",          10, F0_NAMELEN_MASK
def_const "F0_TIME_MASK",        12, F0_TIME_MASK
def_const "F0_TIME_RUN",         11, F0_TIME_RUN
def_const "F0_TIME_IMM",         11, F0_TIME_IMM
def_const "F0_TIME_IMM_PART",    16, F0_TIME_IMM_PART
def_const "F0_TIME_RUN_PART",    16, F0_TIME_RUN_PART

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
# How does code update the dictionary entry? Any word flagged as F0_TIME_MASK ==
# F0_TIME_IMM or F0_TIME_IMM_RUN gets executed immediately instead of being
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
  addl $4, %eax        # address of name as cstr
  movb (%eax), %bl     # get either cstr count or left-most F0 byte
  andl $F0_NAMELEN_BYTE_MASK, %ebx  # clear other flags
  addl $1, %eax        # addr=addr+1 (next character)
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
.int DOCOL, xt_lit, testAlign4Str, xt_countnt, xt_typeTestPass, xt_EXIT
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

# ( nt -- flags ) \ given the nt return the address of flags
#               ascii  , len, flag, label
def_asmword "NT>&FLAGS0",   9,    0, ntTOflags0
  dpop %eax           # address of nt
  addl $4, %eax       # start of cstr (count)
  movl $0, %ebx
  movb (%eax), %bl    # ebx=count
  addl %ebx, %eax     # eax=eax+count+1
  addl $1, %eax
  call eax_align4     # align, as flags is always aligned
  dpush %eax
  NEXT

# ( nt -- flags ) \ given the nt return the xt
#               ascii  , len, flag, label
def_asmword "NT>XT",   9,    0, ntTOxt
  callXt $xt_ntTOflags0
  addl $4, (%ebp) # xt is directly after &flags
  NEXT

# ( nt -- addr count ) \ given the nt return the name
#             ascii  , len, flag, label
def_asmword "NT>NAME",   9,    0, ntTOname
  addl $4, (%ebp) # name is directly after nt
  callXt $xt_count
  NEXT

# ( nt -- flags ) \ get "code field address" (xt) from nt
#         ascii  , len, flag, label
def_doword ">CFA",   4,    0, TOcfa
  .int xt_ntTOxt
  .int xt_EXIT

# ( nt -- flags ) \ get "data field address" from nt
#          ascii  , len, flag, label
def_doword ">DFA",   4,    0, TOdfa
  .int xt_ntTOxt
  .int xt_inc4
  .int xt_EXIT

.align 4
xt_test_ntTOother:
.int DOCOL
# flags: already aligned, cstr=4bytes
.int xt_lit, nt_lit, xt_ntTOflags0, xt_lit, flags_lit, xt_assertEq
# flags: not aligned, cstr=5bytes
.int xt_lit, nt_word, xt_ntTOflags0, xt_lit, flags_word, xt_assertEq
# nt>xt: already aligned
.int xt_lit, nt_lit, xt_ntTOxt, xt_lit, xt_lit, xt_assertEq
# nt>xt: not aligned
.int xt_lit, nt_word, xt_ntTOxt, xt_lit, xt_word, xt_assertEq
.int xt_assertEmpty, xt_lit, test_ntTOotherStr, xt_countnt, xt_typeTestPass
.int xt_EXIT
test_ntTOother:
  callXt $xt_test_ntTOother
  ret

# ( addr count -- nt ) \ finds the string in the dictionary, returning it's nt (name token)
#                      \ or 0 if not found
#           ascii  , len, flag, label
def_asmword "FIND" , 4 ,    0 , find
  movl var_latest, %eax
  dpush %eax    # points to the nt of word we are looking at
  call rrot     # Stack: ( nt addr count )
find_loop:
  call _2dup # ( nt addrf countf addrf countf )
  movl 16(%ebp), %ebx  # nt
  dpush %ebx # ( nt addrf countf addrf countf nt )
  callXt $xt_ntTOstr   # ( nt -- addr count )

  callXt $xt_ceq       # find if strings are equal
  dpop %eax            # if equal, return nt
  cmp $TRUE, %eax      # stack: ( nt addrf countf )
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
# Find DNE
.int xt_lit, testAbcStr, xt_countnt, xt_find, xt_lit, 0, xt_assertEq
# Find SWAP
.int xt_lit, cstr_swap, xt_count, xt_find, xt_lit, nt_swap, xt_assertEq
# Find EXIT
.int xt_lit, cstr_EXIT, xt_count, xt_find, xt_lit, nt_EXIT, xt_assertEq
.int xt_lit, testFindStr, xt_countnt, xt_typeTestPass
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

# ( addr count -- nt ) \ create the next word on the input stream in the dictionary
#             ascii  , len, flag, label
def_asmword "CREATE" ,  6 ,    0 , create
  movl (%ebp), %ecx  # count
  cmp $MAX_WORD_LEN, %ecx  # check if count > max
  jg create_fail
  movl var_here, %edi
  movl var_latest, %eax
  movl %eax, (%edi)      # dict: link to previous
  movl %edi, var_latest  # dict: update latest to this word
  addl $4, %edi          # go to where cstr is stored
  movb %cl, (%edi)       # dict: cstr count
  inc %edi
  call tuck # ( count addr count ) note: doesn't touch edi
  dpush %edi # addr2
  movl %edi, var_here # store edi, as cmove will destroy
  call swap           # stack: ( count addr addr2 count )
  call cmoveUpper     # dict: name string
  dpop %ecx           # count
  movl var_here, %eax   # restore HERE into eax for eax_align4
  addl %ecx, %eax       # add count to here and align
  call eax_align4
  shl $F0_NAMELEN_SHIFT, %ecx  # set up flags
  movl %ecx, (%eax)       # dict: set flags
  addl $4, %eax           # increment eax to point to xt/data/etc
  movl $DOCREATE, (%eax)  # dict: xt for CREATE-ed word
  movl %eax, var_here     # dict: update HERE
  NEXT
create_fail:
  movl $ERR_MAX_WORD_LEN, panicErr
  call *(panic)

.align 4
xt_testCreate:
.int DOCOL
.int xt_here, xt_FETCH
.int xt_lit, testCreatedWord, xt_countnt, xt_create
# HERE has changed
.int xt_here, xt_FETCH, xt_NEQ, xt_assertTrue
# Find the word
.int xt_lit, testCreatedWordUpper, xt_countnt, xt_find
# Flags are as expected (namelen)
.int xt_dup, xt_ntTOflags0, xt_FETCH, xt_lit, 11<<F0_NAMELEN_SHIFT, xt_assertEq
# Stored name is uppercased
.int xt_ntTOname, xt_lit, testCreatedWordUpper, xt_countnt
.int xt_ceq, xt_assertTrue
.int xt_lit, testCreateStr, xt_countnt, xt_typeTestPass
.int xt_EXIT
.align 4
testCreate:
  callXt $xt_testCreate
  ret

# ( u -- ) \ push a value onto the dictionary
#      ascii  , len, flag, label
def_asmword ",",  1,    0, pushDict
  movl var_here, %ebx
  dpop %eax
  movl %eax, (%ebx)  # move value into *here
  addl $4, var_here  # increment here for next value
  NEXT

# ( -- ) \ Set compilation STATE to false
#        ascii  , len,       flag, label
def_asmword "[" ,  1 , F0_TIME_IMM, staterun
  movl $FALSE, var_state
  NEXT

# ( -- ) \ Set compilation STATE to true
#        ascii  , len, flag, label
def_asmword "]" ,  1 ,    0, statecompile
  movl $TRUE, var_state
  NEXT

# ( -- ) \ The forth ":" to define a new word
#      ascii  , len, flag, label
def_doword ":",   1,    0, STARTWORD
  # Create the next word and set correct xt
  .int xt_word # ( addr count )
  .int xt_create, xt_ntTOxt
  # replace DOCREATE with DOCOL
  .int xt_lit, DOCOL, xt_swap, xt_STORE
  .int xt_statecompile # start compiling!
  .int xt_EXIT

# ( -- ) \ end compilation from ":"
#      ascii  , len,        flag, label
def_doword ";",   1, F0_TIME_IMM, ENDWORD
  .int xt_lit, xt_EXIT, xt_pushDict  # compile EXIT
  .int xt_staterun # [ go back to runmode
  .int xt_EXIT

# ( -- ) \ tag currently compiling word as TIME_IMM(ediate)
#          ascii, len, flag, label
def_doword "IMM",   3, F0_TIME_IMM, imm
  .int xt_latest, xt_ntTOflags0, xt_dup
  # Clear TIME flags
  .int xt_F0_TIME_MASK, xt_invert, xt_and, xt_STORE
  # Store TIME_IMM
  .int xt_F0_TIME_IMM, xt_or, xt_STORE
  .int xt_EXIT

# ( -- ) \ forth compliance, same as IMM
#          ascii, len, flag, label
def_doword "IMMEDIATE",   9, F0_TIME_IMM, immediate
  .int xt_imm, xt_EXIT

# ( -- xt ) \ Put the xt of the next word onto the stack.
#          ascii, len, flag, label
def_asmword "'",   1,    0, xt
  lodsl       # do what is in NEXT
  dpush %eax  # but push on stack instead of jmp *(...)
  NEXT

# ( -- ) \ add offset (next cell) to the forth instruction pointer
#   \ performing an unconditional jump. The offset is left
#   \ on the stack.
#              ascii, len, flag, label
def_asmword "BRANCH",   6,    0, branch
  addl (%esi), %esi
  NEXT

# ( offset flag -- offset ) \ perform BRANCH iff flag=0
#               ascii, len, flag, label
def_asmword "0BRANCH",   7,    0, _0branch
  # NOTE: %eax contains xt of current instruction from NEXT. Don't override it!
  dpop %ebx  # count
  cmp $0, %ebx
  je word_branch  # if 0, jump to code in BRANCH above
  lodsl           # otherwise skip the offset
  NEXT

# ( -- ) \ QUIT is the forth interpreter
#            ascii, len, flag, label
def_doword "QUIT",   4,    0, quit
  # set the return stack to rstackMax (clear it)
  .int xt_rstack_max, xt_FETCH, xt_rstackSTORE # clear the rstack
  .int xt_interpret    # interpreter loop
  .int xt_branch, -20  # loop forever

# ( -- ) \ Interpret bytes on the input stream
#                 ascii, len, flag, label
def_asmword "INTERPRET",   9,    0, interpret
  
  NEXT

##############################
# Main Entrypoint
.globl _start
_start:
  movl $heapMin, var_here
  movl $nt_interpret, var_latest
  movl $rstackMax, var_rstack_max
  movl $rstackMax, %esp
  movl $dstackMax, %ebp
  call testSuite
  jmp exit0

.data
.align 128
# The dictionary grows starting here
heapMin: .space 131072  # 128 KiB
heapMax:
