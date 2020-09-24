	.set SYS_EXIT,0
	.set SYS_READ,3
	.set SYS_WRITE,4
	.set SYS_CLOSE,6

	.set F_OPEN,5
.section .data
.section .text
.globl _start
_start:
	movl $222, %ebx
	call exit

exit:
	# ( %ebx:returnCode -- )
	# Exits the program with the return code
	movl __SYS_EXIT, %eax   # system call number for exiting a program
	# %ebx   # return code
	int  $0x80      # kernel vectored interrupt
