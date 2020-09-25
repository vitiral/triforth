# Available registers:
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx
# from above 8bit: al ah  bl bh  cl ch  dl dh
# special: %esp (stack ptr)  %ebp (base ptr)
#          %eip (inst ptr)   %eflags

.code32
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
testStartStr:
	.ascii "Starting Tests\n"
	.equ testStartStrLen,15
passStr:
	.ascii " PASS\n"
	.equ passStrLen,6
testStr:
	.ascii "  Test "
	.equ testStrLen,7

# Test strings
testSimpleIoStr:
	.ascii "testSimpleIo"
	.equ testSimpleIoStrLen,12


.section .bss
	.equ IO_BUFFER_SIZE,128
	.lcomm ioBuffer,IO_BUFFER_SIZE

########################
# Start of our program
.section .text
.globl _start
_start:
	call testSuite
	movl $0, %ebx
	call exit

########################
# Test Suite
testSuite:
	call testSimpleIo
	ret


########################
# IO Basics
# The below functions make the syscalls easier to use,
# and are necessary for the test suite.

flush:
	# purpose: flush the IO buffer to a fd
	# stack: ( -- )
	# reg: %ebx:fd %ecx:&buff %edx:bytesToWrite
	# out: %eax:bytesWritten
	# exit: on IO error rc=errorCode
1:
	# keep writing bytes until bytesToWrite is 0
	call write
	subl %eax, %edx  # sub bytesWritten from bytesToWrite
	addl %eax, %ecx  # add bytesWritten to buffAddr
	cmp $0, %edx
	jne 1b
	ret


write:
	# purpose: write a buffer to a fd
	# stack: ( -- )
	# reg: %ebx:fd %ecx:&buff %edx:bytesToWrite
	# out: %eax:bytesWritten
	# exit: %eax<0 IF exit rc=%eax
	movl $SYS_WRITE, %eax
	int  $LINUX_SYSCALL
	cmp $0, %eax
	jl 1f
	ret
1:
	movl %ecx, %ebx
	call exit
	movl %eax, %ebx
	call exit


exit:
	# purpose: exit the program with the return code
	# stack: ( -- )
	# reg:  %ebx:rc 
	# output: none
	movl $SYS_EXIT, %eax
	int  $LINUX_SYSCALL

reportTestPass:
	# print to stderr that the test passed.
	# stack: ( &buf len -- )
	movl $STDERR, %ebx
	movl $testStr, %ecx
	movl $testStrLen, %edx
	call write

  # TODO: not working either
	# movl 12(%esp), %ecx  # &buf
	# movl 8(%esp), %edx  # len
	movl $testSimpleIoStr, %ecx
	movl $testSimpleIoStrLen, %edx
	call write

	movl $passStr, %ecx
	movl $passStrLen, %edx
	call write
	ret


testSimpleIo:
	# TODO: this is seriously not working...
	# pushl $testSimpleIoStr     # &buf
	# pushl $testSimpleIoStrLen  # len
	call reportTestPass
	# addl $8, %esp
	ret


cmove:
	# move characters from src to dst
	# stack: ( -- )
	# reg: %eax:src %ebx:dst %ecx:count %esi:temporary %edl:temporary
	# warning: this can clobber data if the addresses overlap
	movl $0, %esi  # numMoved
1:
	cmp $0, %ecx # if count=0 then return
	je 2f
	movb (%eax,%esi,1), %dl  # move a byte from src->reg
	movb %dl, (%ebx,%esi,1)  # move a byte from reg->dest
	inc %esi
2:
	ret

