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

	.equ LINUX_SYSCALL,0x80  # linux kernel interrupt vector
	# System call numbers
	.equ SYS_OPEN, 5
	.equ SYS_WRITE, 4
	.equ SYS_READ, 3
	.equ SYS_CLOSE, 6
	.equ SYS_EXIT, 1
	
	# File open options
	.equ O_RDONLY, 0

	#standard file descriptors
	.equ STDIN, 0
	.equ STDOUT, 1
	.equ STDERR, 2

######################################################
### DATA SECTION
######################################################
.section .data
dstackMin: # data stack
	.space 256
dstackMax:
rstackMin: # return stack
	.space 256
rstackMax:

	# This is the _address_ that should be called indirectly when panicing.
	# panicing is like exiting, except our panic code can also do things
	# like print the current stack, etc.
	#
	# Important point: when testing, we will _expect_ panics. This is why we
	# first defined functions like reset/etc
panic: .double 0

# Reserved space for recording an integer error number. Most panic
# implementations will report this integer before exiting.
panic_err: .double 0

# This is used in panic handling code. If reset is called, then
# the rstack will be returned to this value before returning.
#
# Note:reset is typically only called in panic-handling code.
rstackCheckpoint: .double 0
# flags that panic uses
panicFlags: .double 0

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

	movl $sysexit, panic # reset panic to sysexit
	ret
testing_setEbx42:
	movl $42, %ebx
	ret

##############################
# Error Handling Functions

reset: # ( * -- ) ( R: * -- )
	# Clears the data and return stacks. Important: this is used in panic
	# handling code.
	cmp $0, rstackCheckpoint        # if rstackCheckpoint is 0 then we must exit
	                            # since we don't know where to return
	je 1f
	movl rstackCheckpoint, %esp   # reset the rstack to the landing pad
	movl $dstackMax, %ebp            # clear the data stack
	ret                              # return to the top value in new return stack
1:
	movl $RC_INVALIDRESET, %ebx
	jmp sysexit

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

testPanicHandle: # ( * -- )
  # The first thing we need to do is test the panic handling code we just
	# wrote. The steps are:
	# 1. set up our handling code by setting panic, rstackCheckpoint and panic_err
	# 2. call a function which calls another function, which panics.
	# 3. handle the panic by setting panic_err to 0 and resetting
	# 4. assert panic_err is 0 as the code continues.
	movl $testPanicHandle_handle, panic
	movl $42, panic_err
	leal -4(%esp), %eax
	movl %eax, rstackCheckpoint

	# 2.
	call testing_willPanic
	# 4.
	cmp $0, panic_err
	je 1f
	call testFailed
1:
	ret
testPanicHandle_handle:
	# 3.
	movl $0, panic_err
	call reset # note: reset returns to addr at rstackCheckpoint
	jmp unreachable
testing_willPanic:
	call testing_panics
	jmp unreachable
testing_panics:
	call *(panic)
	jmp unreachable

##############################
# Main Entrypoint
.globl _start
_start:
	call testSuite
	jmp exit0
