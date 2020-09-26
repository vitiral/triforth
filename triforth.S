# Available registers:
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx
# from above 8bit: al ah  bl bh  cl ch  dl dh
# special: %esp (stack ptr)  %ebp (base ptr)
#          %eip (inst ptr)   %eflags

#############
# Constants
# Return codes
.equ RC_DBG,99 # 99 problems and debugging ain't one
.equ RC_TFAIL,100
.equ RC_UNREACHABLE,101
.equ RC_INVALIDRESET,102
.equ RC_ARRAYBOUNDS,103
.equ RC_PANIC,104

# Panic Error Codes
.equ ERR_PSTACK_OVERFLOW,0x300
.equ ERR_PSTACK_UNDERFLOW,0x301

# Numbers used in Linux syscalls
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

######################################################
### DATA SECTION
######################################################
# .data must be defined separately from .text (code), so
# the definitions are here. Refer here when you see the
# data used in the code.
.section .data
.equ RSTACK_SIZE, 1024
rstackMin: .space RSTACK_SIZE # return stack
rstackMax:
.equ DSTACK_SIZE, 256
dstackMin: .space DSTACK_SIZE # data stack
dstackMax:
.equ IO_BUFFER_SIZE, 128
ioBuffer: .space 128
ioBufferLen: .int 0  # number of chars in ioBuffer

# This is the _address_ that should be called indirectly when panicing.
# panicing is like exiting, except our panic code can also do things
# like print the current stack, etc.
#
# Important point: when testing, we will _expect_ panics. This is why we
# first defined functions like reset/etc
panic: .int defaultPanic
# Reserved space for recording an integer error number. Most panic
# implementations will report this integer before exiting.
panicErr: .int 0
# This is used in panic handling code. If reset is called, then
# the rstack will be returned to this value before returning.
#
# Note:reset is typically only called in panic-handling code.
rstackCheckpoint: .int 0
# flags that panic uses
panicFlags: .int 0

emitFd: .int 2  # STDERR
emitBuf: .int 0 # scratch space for emit

##############################
# Text Strings: unaligned strings for IO buffer
testPrefixStr: .ascii "**TEST \0"
testPassStr: .ascii " PASS\n\0"
panicStr: .ascii "!! PANIC ENCOUNtERED\n\0"

# Test Names
testIoBufferStr: .ascii "testIoBuffer\0"

.align 8

######################################################
### CODE SECTION
######################################################
.section .text

##############################
# Core Testing Functions
testSuite: # ( -- )
  # We will call this to run all our assembly-level tests.
  # As we write tests they should be put here.
  call testAsserts
  call testIndirectCall
  call testPanicHandle
  call testMemLocations
  call testDsPushPop
  call testStackBasic
  call testEmit
  call testCountNt
  call testIoBuffer
  ret

assertEaxEbxEq:
  cmp %eax, %ebx
  je 1f
  jmp testFailed
1:
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

  movl $defaultPanic, panic # reset panic to sysexit
  movl $0, rstackCheckpoint
  ret
testing_setEbx42:
  movl $42, %ebx
  ret

##############################
# Error Handling Functions
sysexit: # %ebx:rc ( -- )
  # exit the program with the return code in %ebx
  # This is our first function. We put our forth-style type signature at the
  # top, with register inputs/outputs outside of the parens
  movl $SYS_EXIT, %eax
  int  $LINUX_SYSCALL

unreachable:
  # Call to distinguish unrechable code. exits with RC_UNREACHABLE
  movl $RC_UNREACHABLE, %ebx
  jmp sysexit

testFailed:
  # Call to distinguish unrechable code. exits with RC_TFAIL
  movl $RC_TFAIL, %ebx
  jmp sysexit

dbgexit: # ( -- )
  # Exits immediately with rc=99
  # Mainly used in debugging.
  movl $RC_DBG, %ebx
  jmp sysexit

exit0: # ( -- )
  # Exits immediately with rc=0
  movl $0, %ebx
  jmp sysexit

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

depth: # ( -- u )
  # returns the depth of the stack
  movl $dstackMax, %eax
  subl %ebp, %eax
  dpush %eax
  ret

assertDstackEmpty:
  # Assert that the datastack is empty
  movl %ebp, %eax
  movl $dstackMax, %ebx
  call assertEaxEbxEq
  ret

testDsPushPop:
  # test push/pop and overflow/underflow checking
  call assertDstackEmpty

  # push/pop works
  dpush $42
  dpop %eax
  movl $42, %ebx
  call assertEaxEbxEq
  call assertDstackEmpty

  # push2 / pop2 works
  dpush $43
  dpush $44
  dpop %eax
  movl $44, %ebx
  call assertEaxEbxEq
  dpop %eax
  movl $43, %ebx
  call assertEaxEbxEq
  call assertDstackEmpty

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
  ret
testDsPushPop_fail:
  call testFailed
testing_underflow:
  call assertDstackEmpty
  dpush $4
  dpop %eax
  dpop %eax # underflow
  call unreachable
testing_overflow:
  call assertDstackEmpty
  movl $dstackMin, %ebp
  addl $4, %eax # overflows when <=, so will still overflow 4 above.
  dpush $4  # overflow
  call unreachable

swap: # ( a b -- b a )
  # Forth SWAP
  dpop %ebx
  dpop %eax
  dpush %ebx
  dpush %eax
  ret

drop: # ( a -- )
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

  ret

emit: # ( c -- )
  # emits a single character to emitFd
  dpop %eax
  movb %al, emitBuf
  movl emitFd, %ebx   # fd
  movl $emitBuf, %ecx # &buf
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

dot: # ( n -- )
  # forth '.', emit the value on the stack.
  dpop %eax
1:
  # TODO: implement this
  call unreachable

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

defaultPanic: # ( -- )
  # The default panic implemention uses the above tools to ouput
  # relevant panic messages.
  dpush $panicStr
  call countnt
  call typeTestPass
  # TODO: emit the stack
  movl $RC_PANIC, %ebx
  jmp sysexit

##############################
# Main Entrypoint
.globl _start
_start:
  movl $rstackMax, %esp
  movl $dstackMax, %ebp
  call testSuite
  jmp exit0
