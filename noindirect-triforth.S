# Available registers:
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx
# from above 8bit: al ah  bl bh  cl ch  dl dh
# special: %esp (stack ptr)  %ebp (base ptr)
#          %eip (inst ptr)   %eflags

#############
# Constants
	# Return codes
	.equ RC_TFAIL,100
	.equ RC_UNREACHABLE,101

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

.section .data
dstackMin: # data stack
	.space 256
dstackMax:
rstackMin: # return stack
	.space 256
rstackMax:

panic:
	# This is the _address_ that should be called indirectly when panicing.
	# panicing is like exiting, except our panic code can also do things
	# like print the current stack, etc.
	#
	# Important point: when testing, we will _expect_ panics. This is why we
	# first defined functions like reset/etc
	.long sysexit # default (for now) is to just call sysexit
errno:
	# Reserved space for recording an integer error number. Most panic
	# implementations will report this integer before exiting.
	.long 0
rstackCheckpoint:
	# This is used in panic handling code. If reset is called, then
	# the rstack will be returned to this value before returning.
	#
	# Note:reset is typically only called in panic-handling code.
	.long 0
panicFlags: # flags that panic uses
	.long 0

.align 8
testPanicHandle_handle:
	call dbgexit
	# 3.
	movl $0, errno
	call reset # note: reset returns to addr at rstackCheckpoint
	call unreachable

.section .text
########################
# Testing
# Before we even start running our program, we are going to fill
# out our test infrastructure

testSuite: # ( -- )
	# We will call this to run all our assembly-level tests.
	# As we write tests they should be put here.
	call testPanicHandle
	ret

reset: # ( * -- ) ( R: * -- )
	# Clears the data and return stacks. Important: this is used in panic
	# handling code.
	cmp $0, (rstackCheckpoint)        # if rstackCheckpoint is 0 then we must exit
	                            # since we don't know where to return
	je 1f
	movl rstackCheckpoint, %esp   # reset the rstack to the landing pad
	movl $dstackMax, %ebp             # clear the stack
	ret                              # return to the top value in new return stack
1:
	movl $22, %ebx
	call sysexit


unreachable:
	# Call to distinguish unrechable code. exits with RC_UNREACHABLE
	movl $RC_UNREACHABLE, %ebx
	call sysexit

testFailed:
	# Call to distinguish unrechable code. exits with RC_TFAIL
	movl $RC_TFAIL, %ebx
	call sysexit

sysexit: # %ebx:rc ( -- )
	# exit the program with the return code in %ebx
	# This is our first function. We put our forth-style type signature at the
	# top, with register inputs/outputs outside of the parens
	movl $SYS_EXIT, %eax
	int  $LINUX_SYSCALL

dbgexit: # ( -- )
	# Exits immediately with rc=222
	movl $222, %ebx
	call sysexit

exit0: # ( -- )
	# Exits immediately with rc=0
	movl $0, %ebx
	call sysexit


testPanicHandle: # ( * -- )
  # The first thing we need to do is test the panic handling code we just
	# wrote. The steps are:
	# 1. set up our handling code by setting panic, rstackCheckpoint and errno
	# 2. call a function which calls another function, which panics.
	# 3. handle the panic by setting errno to 0 and resetting
	# 4. assert errno is 0 as the code continues.
	movl $testPanicHandle_handle, panic
	movl $42, errno
	# Move esp-4 into rstackCheckpoint. This is where the rstack will be reset to.
	leal -4(%esp), %eax
	movl %eax, rstackCheckpoint

	# 2.
	# call *(panic)
	# FIXME: cannot get indirect calling to work :(
	movl $testPanicHandle_handle, panic
	call *(panic)
	call dbgexit

	call exit0
	call testing_willPanic
	cmp $0, (errno)
	je 1f
	call testFailed
1:
	# test passed
	ret
	
testing_willPanic:
	call testing_panics
	call unreachable
testing_panics:
	call *(panic)
	call unreachable
# 	.text
# 	.align 4
# testPanicHandle_handle:
# 	call dbgexit
# 	# 3.
# 	movl $0, errno
# 	call reset # note: reset returns to addr at rstackCheckpoint
# 	call unreachable


########################
# Start of our program
.globl _start
_start:
	movl $dstackMax, %ebp        # ebp is our SP (data stack pointer)
	movl $rstackMax, %esp       # esp is our R (return stack pointer)
	call testSuite
	movl $0, %ebx
	call sysexit

# dpush: # ( -- %eax )
# 	# push %eax onto the data stack
# 	subl $4, %ebp      # grow stack down
# 	cmp $dstackMin, %ebp  # check for stack overflow
# 	jl 1f
# 	movl %eax, (%ebp)  # put value in
# 	ret
# 1:
# 	movl $2, %ebx
# 	call sysexit
# 
# dpop: # ( u -- ) %eax=u
# 	# pop a value in the data stack into %eax
# 	cmp $dstackMax, %ebp # check for stack underflow
# 	jge 1f
# 	movl (%ebp), %eax # move the value in to return
# 	addl $4, %ebp     # shrink stack upwards
# 	ret
# 1:
# 	movl $3, %ebx
# 	call *(exit)

