# REGISTERS: The below is a helpful register reference.
# general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
# from above 16bit:  ax     bx     cx     dx   none  none
# from above 8bit: al ah  bl bh  cl ch  dl dh  none  none
# special:  %esp: stack ptr, our return stack ptr (RSP)
#           %ebp: base ptr, our parameter stack ptr (PSP)
#           %eip: cpu inst ptr
#           %esi: our forth xt-inst ptr
#           %eflags: not used directly here
#
# basic types:
# o ot (One byte)
# w wot (tWO bytes)
# f fot (FOur bytes)
# g got (eiGht bytes)

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
.equ RC_TODO,98
.equ RC_DBG,99 # 99 problems and debugging ain't one, `call dbgexit`
.equ RC_TFAIL,100
.equ RC_UNREACHABLE,101
.equ RC_INVALIDRESET,102
.equ RC_ARRAYBOUNDS,103
.equ RC_PANIC,104
.equ RC_STACK_UNDERFLOW,105
.equ RC_STACK_OVERFLOW,106
.equ RC_STACK_OVERFLOW,106
.equ RC_ASM_INVARIANT,107

# Our Panic Error Codes
.equ ERR_SEE_MSG,0x001
.equ ERR_OKAY,0xBEEF # okay
.equ ERR_PSTACK_OVERFLOW,0x300
.equ ERR_PSTACK_UNDERFLOW,0x301
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

################
## Data layout
# |-rstack----|-pstack-|-"code"-------------------|-memory---...
#
# Code contains _only_ code (not the word dictionary). The word dictionary is
# a normal linked-list, except the name, flags and type is all kept inside of
# "memory" and so can be dropped once the "compilation" is complete.
#
# 64k of "memory" is initially reserved, and ~64k for the remaining blocks.

######################################################
### DATA SECTION
# It's better to define most .data separately from .text (code), so the
# definitions are here. Refer here when you see the data used in the code.
.section .data
.equ RSTACK_SIZE, 0x2000
rstackMin: .space RSTACK_SIZE # return stack
rstackMax:
.equ PSTACK_SIZE, 0x100
pstackExtra: .space 0x20      # guaranteed space above the pstack
pstackMin: .space PSTACK_SIZE # data stack
pstackMax:
registerSnapshot: .space 24

errno: .int 0
# Address that should be called when panicing, can be redirected when testing.
panic: .int 0
# Specefic error code encountered when panicing.
panicErr: .int 0
# Used in `reset` to return from a panic in testing.
rstackCheckpoint: .int 0

emitFd: .int STDERR # where to EMIT and TYPE to
.equ EMIT_BUFFER_SIZE,16
keyFd: .int STDIN   # where KEY/etc gets input from

##################################################################################
#### PURE ASSEMBLY                                                            ####
##################################################################################

return:
  # Functions can conditionally jump here to return
  ret

sysexit: # %ebx:rc ( -- )
  # Directly call SYS_EXIT using value in ebx
  movl $SYS_EXIT, %eax
  int  $LINUX_SYSCALL

.macro EXIT rc
  movl \rc, %ebx
  jmp sysexit
.endm

asmInvariantViolated: EXIT $RC_ASM_INVARIANT

.equ offset_eax, 0x00
.equ offset_ebx, 0x04
.equ offset_ecx, 0x08
.equ offset_edx, 0x0C
.equ offset_edi, 0x10
.equ offset_esi, 0x14

.macro SNAPSHOT_REG r
  movl %\r, registerSnapshot+offset_\r
.endm

snapshotRegisters:
  SNAPSHOT_REG eax
  SNAPSHOT_REG ebx
  SNAPSHOT_REG ecx
  SNAPSHOT_REG edx
  SNAPSHOT_REG edi
  SNAPSHOT_REG esi
  ret

.macro RESTORE_REG r
  movl registerSnapshot+offset_\r, %\r
.endm

restoreRegisters:
  RESTORE_REG eax
  RESTORE_REG ebx
  RESTORE_REG ecx
  RESTORE_REG edx
  RESTORE_REG edi
  RESTORE_REG esi
  ret

syswrite: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
  # Direct call to SYS_WRITE syscall with error checking.
  cmp $0, errno # if errno is not zero, do nothing
  jne return
  movl $SYS_WRITE, %eax
  int $LINUX_SYSCALL
  cmpl $0, %eax # error if lt 0
  jge return
  movl %ebx, errno # error
  ret

countntEcx: # %ecx:ntstr ( -- ) %ecx:ntstr %edx:count
  # Count the number of bytes in a null-terminated string
  movl $-1, %edx
countntEcx_loop:
  inc %edx
  cmpb $0, (%ecx, %edx, 1)
  jne countntEcx_loop
  ret

pntNtStr: # %ecx:ntstr ( -- )
  # Print a null terminated string
  # Does not change any registers
  call countntEcx
  movl $STDERR, %ebx
  call syswrite
  ret

.macro SAFE_PNT_NT_STR s
  # Safely print a null terminated string
  # (Safe means no registers changed)
  call snapshotRegisters
  movl \s, %ecx
  call pntNtStr
  call restoreRegisters
.endm

ebx_toascii: # ( u -- c )
  # Convert a number (typically 1-16) into ascii equivalent
  cmpl $9, %ebx
  jg ebx_toascii_gt9
  # number <= 9
  addl $'0', %ebx
  ret
ebx_toascii_gt9:
  subl $10, %ebx
  cmpl $23, %ebx
  jg asmInvariantViolated # ebx not [0, 24)
  addl $'A', %ebx
  ret

ebxUx: # %ebx:f ( 8o -- 8o ) %edx:right-aligned-count
  # Store ebx as hex ascii into (up to) the first 8 bytes of the stack.
  # The caller must reserve this space.
  # Uses: ebx, ecx, edx
  cmpl $0, %ebx
  je ebxUx_zero
  movl $7, %edx      # %edx=index to store character
  movl %ebx, %ecx    # %ecx=remaining n
ebxUx_loop:
  movl %ecx, %ebx
  andl $0xF, %ebx  # ebx=ebx%0x10
  call ebx_toascii
  movb %bl, (%ebp, %edx, 1)
  dec %edx
  shrl $4, %ecx    # ecx=ecx/0x10
  jnz ebxUx_loop
  neg %edx      # edx=-edx+8 (count)
  addl $8, %edx
  ret
ebxUx_zero:
  movl $'0', 7(%ebp)
  movl $1, %edx
  ret

ebxUxPadded: # %ebx:f ( 8o -- 8o )
  # Store ebx as hex ascii into the first 8 bytes of the stack, pading any
  # remaining bytes. The caller must reserve this space.
  # Uses: ebx, ecx, edx
  # call ebxUx
  EXIT $RC_TODO

pntEbxUx: # %ebx:f ( -- )
  # Uses: eax, ebx, ecx, edx
  subl $8, %ebp  # reserve space for bytes buffer
  call ebxUx  # edx=bytesToWrite
  movl $STDERR, %ebx  # fd
  leal 8(%ebp), %ecx  # &buff=&end-count
  subl %edx, %ecx
  call syswrite
  addl $8, %ebp
  ret

.macro PNT_UX v
  movl \v, %ebx
  call pntEbxUx
.endm


##################################################################################
#### STACK                                                                    ####
##################################################################################
pushDsInvalid:
  # Checks to make sure that the datastack hasn't overflowed.
  # If it has, re-increments and panics with appropriate error.
  addl $4, %ebp
  movl $ERR_PSTACK_OVERFLOW, panicErr
  call *(panic)
  EXIT $RC_UNREACHABLE

.macro DPUSH reg
  subl $4, %ebp
  cmpl $pstackMin, %ebp
  jl pushDsInvalid
  movl \reg , (%ebp)
.endm

popDsInvalid:
  # Checks to make sure that the datastack hasn't underflowed.
  # If it has, re-decrements and panics with appropriate error.
  subl $4, %ebp
  movl $ERR_PSTACK_UNDERFLOW, panicErr
  call *(panic)
  EXIT $RC_UNREACHABLE

.macro DPOP reg
  movl (%ebp) , \reg
  cmpl $pstackMax, %ebp
  jge popDsInvalid
  call checkPopDs
  addl $4, %ebp
.endm

##############################
# Tests
mainStartNtStr: .ascii "Starting Triforth...\n\0"
.align 4

##############################
# Main Entrypoint
# This exists just so the 
.globl _start
_start:
  movl $rstackMax, %esp  # clear rstack
  movl $pstackMax, %ebp  # clear pstack
  # SAFE_PNT_NT_STR $mainStartNtStr
  PNT_UX $0x42
  SAFE_PNT_NT_STR $mainStartNtStr
  EXIT $0
