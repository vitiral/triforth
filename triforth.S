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
.equ O_RDONLY, 0

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

# Address that should be called when panicing, can be redirected when testing.
panic: .int defaultPanic
# Specefic error code encountered when panicing.
panicErr: .int 0
# Used in `reset` to return from a panic in testing.
rstackCheckpoint: .int 0
# flags that panic uses
panicFlags: .int 0
  .equ PANIC_FLAG_PANICING,1  # TODO: use to avoid recursion during panicing

emitFd: .int STDERR # where to EMIT and TYPE to
.equ EMIT_BUFFER_SIZE,16
emitBuffer: .space EMIT_BUFFER_SIZE # scratch space for emit and DOT
emitBufferMax:
BASE: .int 10   # base for default number conversions

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

# Test Names
testIoBufferStr: .ascii "testIoBuffer\0"
testDOTStr: .ascii "testDOT\0"
testDOTsStr: .ascii "testDOTs\0"
testDumpInfoStr: .ascii "testDumpInfo\0"

.align 8

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
  call testCmove
  call assertCleanState
  call testDumpInfo
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


##############################
# Exiting functions
# These are for quickly exiting our program with relevant return codes.

sysexit: # %ebx:rc ( -- )
  # Directly call SYS_EXIT
  movl $SYS_EXIT, %eax
  int  $LINUX_SYSCALL

testFailed: # The test failed
  movl $RC_TFAIL, %ebx
  jmp sysexit

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

modDIV: # ( u:a u:b -- u u )
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
  call modDIV
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
  dpop %eax
  ret

_2drop: # ( a b -- )
  dpop %eax
  dpop %eax
  ret

rot: # ( a b c -- b c a )
  # Forth ROT, rotates left
  dpop %ecx
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %ecx
  dpush %eax
  ret

rotr: # ( a b c -- c a b )
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
  call modDIV
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

  call rot # ( 0 1 2 )

  movl (%ebp), %eax
  movl $2, %ebx
  call assertEaxEbxEq
  movl 4(%ebp), %eax
  movl $1, %ebx
  call assertEaxEbxEq
  movl 8(%ebp), %eax
  movl $0, %ebx
  call assertEaxEbxEq

  call rotr # ( 2 0 1 )

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
  movl BASE, %eax
  dpush %eax
  call DOTbase
  dpush $' '
  call emit
  ret

DOTbase: # ( n u:base -- )
  # emit the number using the given base.
  # TODO: deal with negatives
  dpush $0 # count: characters in buffer
  call rotr # ( count n base )
1:
  call dup  # ( count n base base )
  call rotr # ( count base n base )
  call modDIV # ( count base quot rem )
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


testCmove:
  # TODO
  ret


##############################
# Debug Helpers
# These will help us as we develop to view the state of the stack and registers
# anywhere in our program. They will also give us more helpful error messages
# when things fail.

dumpInfo:
  # Registers
  movl %eax, registerSnapshot
  movl $registerSnapshot, %eax
  movl %ebx, 4(%eax)
  movl %ecx, 8(%eax)
  movl %edx, 12(%eax)
  movl %edi, 16(%eax)
  movl %esi, 20(%eax)
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
# Forth Dictionary
# The forth dictionary is our most important datatype. Fundamentally it is singly-linked
# list. This is a fundamentally simple datatype where each "item" within the list contains
# a link to the previous item, like this:
#
#     ...
#     link | ... data ...
#      ^
#      |
#     link | ... data ...
#
# The dictionary in our forth (and most forths) is such a list, and the data is
# arrayed like this:
#
#     link | info | name | xt | code/data ...
# 
# The various fields above are;
# - link: this is the link to the previous entry in the LL, or 0 if this is the end.
# - info: N bytes of information containing:
#   - TODO: fill this out
# - name: a ctstr (counted str) with the count as the first byte and the string as the rest.
#    we pad this string so it aligns with 4 bytes.
# - xt: the EXECUTION TOKEN, a core datatype in forth we will get to.
# - code/data: the code and data that follows the xt will depend on the word. We will get into
#   more detail later.
#
# The basic execution model of forth is that the "interpreter" will simply execute the
# xt's in the "code" section of words. Those xt's tokens may in turn execute a
# list of their own xt's, etc.  Unlike languages like C, no variables have to
# be passed between the xt-calls, because every xt will simply pop/push from
# the data stack.
#
# Unlike assembly, the xt we are executing will be stored in %esi, which is helpful
# because it allows us to use instructions normally reserved for strings to quickly
# increment/decrement it.
#

  # .macro NEXT
  # # The NEXT macro increments the xt we are executing and the executes it.
  # # Every forth primitive has to be ended by NEXT. It's kind of like incrementing
  # # the %eix when instructions are run.
  # lodsl %eax // %eax=%esi; %esi+=4
  # jmp *(%eax) // indirect jump
  # .endm


# Just like assembly, before we execute a new xt, we need to store our return address
# so that the interpreter can continue executing the rest of our words. The operations
# to modify the return stack are simply `pushl` and `popl`, so we will use those directly.

# DOCOL:
#   pushl %esi  # push esi onto the return stack
#   addl $4, %eax # %eax points to our own xt (from NEXT), increment it to point into code
#   movl %eax, %esi # and set esi to point to it instead.
#   NEXT
# 
# 
# EXIT:
#   popl %esi # pop return code into esi
#   NEXT

  .set LINK, 0

  .macro defcode name, namelen, flags=0, label
  .section .rodata
  .align 4
  .globl name_\label
name_\label :
  .int link               # link to previous
  .set link,name_\label   # update link to self
  .int \flags             # flags for the word
  .byte \namelen          # the length of the name
  .ascii "\name"          # the ascii name itself
  .align 4                # padding to next 4 byte boundary
xt_\label :
  .int \label // codeword
  .endm

##############################
# Main Entrypoint
.globl _start
_start:
  movl $rstackMax, %esp
  movl $dstackMax, %ebp
  call testSuite
  jmp exit0
