// general 32bit:    %eax   %ebx   %ecx   %edx  %edi  %esi
// from above 16bit:  ax     bx     cx     dx   none  none
// from above 8bit: al ah  bl bh  cl ch  dl dh  none  none
// special:  %esp: stack ptr, our return stack ptr (RSP)
//           %ebp: base ptr, our parameter stack ptr (PSP)
//           %eip: cpu inst ptr
//           %esi: our forth xt-inst ptr
//           %eflags: not used directly here
//
// basic types:
// o ot (One byte)
// w wot (tWO bytes)
// f fot (FOur bytes)
// g got (eiGht bytes)

//#############################
// Constants
// It's better to define these at the top of the file. Use these as a reference
// as you encounter them.
.equ LINUX_SYSCALL,0x80  # linux kernel interrupt vector
.equ SYS_OPEN, 5
.equ SYS_WRITE, 4
.equ SYS_READ, 3
.equ SYS_CLOSE, 6
.equ SYS_EXIT, 1

// File open options
.equ O_RDONLY, 0x0000  # open for reading only
.equ O_WRONLY, 0x0001  # open for writing only
.equ O_RDWR,   0x0002  # open for reading and writing

// Standard file descriptors
.equ STDIN, 0
.equ STDOUT, 1
.equ STDERR, 2

// Our Return codes
.equ RC_TODO, 98
.equ RC_DBG, 99 # 99 problems and debugging ain't one, `call dbgexit`
.equ RC_TESTFAIL, 100
.equ RC_UNREACHABLE, 101
.equ RC_INVALIDRESET, 102
.equ RC_ARRAYBOUNDS, 103
.equ RC_PANIC, 104
.equ RC_STACK_UNDERFLOW, 105
.equ RC_STACK_OVERFLOW, 106
.equ RC_STACK_OVERFLOW, 106
.equ RC_ASM_INVARIANT, 107

// Our Panic Error Codes
.equ ERR_SEE_MSG, 0x001
.equ ERR_OKAY, 0xBEEF # okay
.equ ERR_PSTACK_OVERFLOW, 0x300
.equ ERR_PSTACK_UNDERFLOW, 0x301
.equ ERR_ASCII_BOUNDS, 0x302
.equ ERR_MAX_WORD_LEN, 0x303
.equ ERR_WORD_MISSING, 0x304
.equ ERR_NUMBER_INVALID, 0x305
.equ ERR_UNKNOWN_WORD, 0x306
.equ ERR_STATE_REDUNDANT, 0x307
.equ ERR_STACK_UNDERFLOW, 0x308
.equ ERR_WORD_NOT_FOUND, 0x309
.equ ERR_INVALID_FLAG, 0x310
.equ ERR_INVALID_CHAR, 0x311
.equ ERR_STACK_NOT_EMPTY, 0x312
.equ ERR_STATE_INCORRECT, 0x313
.equ ERR_EMIT_ERR, 0x314
.equ ERR_NULL_NAMETOKEN, 0x315
.equ ERR_ERRNO, 0x316

.equ FALSE, 0
.equ TRUE, -1

//###############
// Data layout
// |-rstack----|-pstack-|-"code"-------------------|-memory---...
//
// Code contains _only_ code (not the word dictionary). The word dictionary is
// a normal linked-list, except the name, flags and type is all kept inside of
// "memory" and so can be dropped once the "compilation" is complete.

################################################################################
#### DATA SECTION -- mutable memory must go here.                           ####
################################################################################
.section .data
.equ RSTACK_SIZE, 0x2000
rstackMin: .space RSTACK_SIZE # return stack
rstackMax:
.equ PSTACK_SIZE, 0x100
pstackExtra: .space 0x20      # guaranteed space above the pstack
pstackMin: .space PSTACK_SIZE # data stack
pstackMax:
registerSnapshot: .space 0x20

pntFd: .int STDERR # where to EMIT and TYPE to
.equ EMIT_BUFFER_SIZE, 16
keyFd: .int STDIN   # where KEY/etc gets input from
errno: .int 0
panic: .int panicDefault # function to call when panicing
panicErr: .int 0  # error to print while panicing

# ONLY USED IN TESTING
testingNtStr: .int 0
panicRestore: .int 0 

.section .text

################################################################################
#### PURE ASSEMBLY                                                          ####
################################################################################
// The following are pure assembly functions whose sole purpose is to make
// developing in assembly easier. None of these will be linked directly to
// forth words (although some are used in their implementations).
//
// More importantly, these make it much easier to debug both assembly and forth
// as you are writing the bootstrapper. Feel free to implement them yourself and
// then look at how I have done it. The basic functionality we need is to be
// able to print registers and the stack, along with an arbitrary number. This
// will allow us to more easily reason about code that is being executed.
//
// Finally, we develop a way to "panic", printing out debug information in the
// process. We use indirection so that we can expect panics in our tests.

.macro ZERO reg
  // Zero a register. Uses xor which is more compact than mov
  xor \reg, \reg
.endm

.macro JRZ reg, loc
  // Jump to location if the register is zero.  This uses `test`, which does an
  // AND that only mutates the eflags register.
  test \reg, \reg
  jz \loc
.endm

.macro JRNZ reg, loc
  // Jum to location if the register is not zero.
  test \reg, \reg
  jnz \loc
.endm

return:
  // Functions can conditionally jump here to return
  // I.e.: jnz return
  // instead of having to define a special return memory location
  // within the function.
  ret

sysexit: # %ebx:rc ( -- )
  // Directly call SYS_EXIT using value in ebx
  movl $SYS_EXIT, %eax
  int  $LINUX_SYSCALL

.macro EXIT rc
  movl \rc, %ebx
  jmp sysexit
.endm

asmInvariantViolated: EXIT $RC_ASM_INVARIANT
unreachable: EXIT $RC_UNREACHABLE

// Snapshotting registers allows for debug printing to happen without affecting
// the assembly you are debugging.
.equ offset_eax, 0x00
.equ offset_ebx, 0x04
.equ offset_ecx, 0x08
.equ offset_edx, 0x0C
.equ offset_edi, 0x10
.equ offset_esi, 0x14

.macro SNAPSHOT_REG r
  movl %\r , (registerSnapshot+offset_\r)
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
  movl (registerSnapshot + offset_\r) , %\r
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
  # Direct call to SYS_WRITE syscall
  movl $SYS_WRITE, %eax
  int $LINUX_SYSCALL
  ret

syswriteall: # %ebx:fd %ecx:&buff %edx:bytesToWrite ( -- )
  # Writes to the fd, making sure that all bytes are written.
  cmp $0, errno # if errno is not zero, do nothing
  jne return
  call syswrite
  cmpl $0, %eax # error if lt 0
  jl syswriteall_error
  subl %eax, %edx # edx=edx-eax  # OPP src, dest
  jz return # if edx=0 we are done
  addl %ecx, %eax
  jmp syswriteall # else, run again
syswriteall_error:
  movl %eax, errno # error
  ret

countntEcx: # %ecx:ntstr ( -- ) %ecx:ntstr %edx:count
  // Count the number of bytes in a null-terminated string
  // Writes the count to edx so it can be used in syswrite
  movl $-1, %edx
countntEcx_loop:
  inc %edx
  cmpb $0, (%ecx, %edx, 1)
  jne countntEcx_loop
  ret

pntEcxNtStr: # %ecx:ntstr ( -- )
  // Print a null terminated string
  // Does not change any registers
  call countntEcx   # edx=bytesToWrite
  movl pntFd, %ebx
  call syswriteall
  ret

asciiStr: .ascii "~~~~~~~~~\t\n~~\r~~"  # 0x0-F
          .ascii "~~~~~~~~~~~~~~~~"     # 0x10-1F
          .ascii " ! \"$%&'()*+,-./"    # 0x20-2F
          .ascii "0123456789:;<=>?"     # 0x30-3F
          .ascii "@ABCDEFGHIJKLMNO"     # 0x40-4F
          .ascii "PQRSTUVWXYZ[\\]^_"    # 0x50-5F
          .ascii "`abcdefghijklmno"     # 0x60-6F
          .ascii "pqrstuvwxyz{|}~"      # 0x70-7E
.align 4
pntEcxAscii: # %ecx:byte ( -- )
  // Print the ascii value in Ecx
  // writesys requires a pointer so this allows printing single characters.
  movl pntFd, %ebx      # fd
  addl $asciiStr, %ecx  # &buff
  movl $1, %edx         # bytesToWrite
  call syswriteall
  ret

.macro PNT_ASCII a
  movl \a , %ecx
  call pntEcxAscii
.endm

.macro PNT_NT_STR s
  // Print a null-terminated string.
  movl \s, %ecx
  call pntEcxNtStr
.endm

ebx_toascii: # ( u -- c )
  // Convert a number (typically 1-16) into ascii equivalent
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
  // Store ebx as hex ascii into (up to) the first 8 bytes of the stack.
  // The caller must reserve this space.
  // The resulting count is stored in edx so it can be used in syswrite.
  JRZ %ebx, ebxUx_zero
  movl $7, %eax     # %eax=index to store character
  movl %ebx, %ecx   # %ecx=remaining n
  ZERO %edx
ebxUx_loop:
  cmp $7, %edx
  jge asmInvariantViolated
  cmp $0, %ecx
  jl asmInvariantViolated

  movl %ecx, %ebx
  andl $0x0F, %ebx  # ebx=ebx % 0x10
  call ebx_toascii
  movb %bl, (%ebp, %eax, 1)
  inc %edx
  dec %eax
  shrl $4, %ecx    # ecx=ecx / 0x10
  jnz ebxUx_loop
  ret
ebxUx_zero:
  movb $'0', 7(%ebp)
  movl $1, %edx
  ret

ebxUxPad: # %ebx:f ( 8o -- 8o ) %edx:count ( always 8 )
  // Store ebx as hex ascii into the first 8 bytes of the stack, pading any
  // remaining bytes.
  // The caller must reserve this space.
  // The resulting count is stored in edx so it can be used in syswrite.
  call ebxUx
  # Set eax=place to start putting zeros
  leal 8(%ebp), %eax
  subl %edx, %eax
  movl $8, %edx # always return count=8
ebxUxPad_loop:
  cmp %ebp, %eax
  jle return
  dec %eax
  movb $'0', (%eax)
  jmp ebxUxPad_loop

pntEbxUx: # %ebx:f ( -- )
  subl $8, %ebp       # reserve space for bytes buffer
  call ebxUx          # edx=bytesToWrite
  movl pntFd, %ebx    # fd
  leal 8(%ebp), %ecx  # &buff=&end-count
  subl %edx, %ecx
  call syswriteall
  addl $8, %ebp       # unreserve space for bytes buffer
  ret

pntEbxUxPretty: # %ebx:f ( -- )
  subl $8, %ebp  # reserve space for bytes buffer
  call ebxUxPad
  movl pntFd, %ebx    # ebx=fd
  movl %ebp, %ecx     # ecx=&buf
  movl $4, %edx       # edx=bytes to write
  call syswriteall
  PNT_ASCII $'_'      # ebx=fd
  leal 4(%ebp), %ecx  # ecx=&buf second half
  movl $4, %edx       # edx=bytes to write
  call syswriteall
  addl $8, %ebp  # unreserve stack
  ret

.macro PNT_NTSTR s
  movl \s, %ecx
  call pntEcxNtStr
.endm

.macro PNT_UX v
  movl \v, %ebx
  call pntEbxUx
.endm

.macro PNT_UX_NL v
  PNT_UX \v
  PNT_ASCII $'\n'
.endm

.macro PNT_UX_PRETTY v
  movl \v, %ebx
  call pntEbxUxPretty
.endm

.macro PNT_UX_PRETTY_NL v
  PNT_UX_PRETTY \v
  PNT_ASCII $'\n'
.endm

dbgStartNtStr: .ascii "DBG(\0"
dbgRegNtStr_eax: .ascii " eax=\0"
dbgRegNtStr_ebx: .ascii " ebx=\0"
dbgRegNtStr_ecx: .ascii " ecx=\0"
dbgRegNtStr_edx: .ascii " edx=\0"
dbgRegNtStr_edi: .ascii " edi=\0"
dbgEndNtStr: .ascii ")\n\0"
.align 4
.macro DBG_REG r
  PNT_NTSTR $dbgRegNtStr_\r
  PNT_UX_PRETTY registerSnapshot+offset_\r
.endm

dbgRegisters: # prints the snapshotted registers
  DBG_REG eax
  DBG_REG ebx
  DBG_REG ecx
  DBG_REG edx
  DBG_REG edi
  ret

.macro DBG v
  // Allows us to debug registers at varioius points
  // Usage: DBG $0x444 will print something like:
  // DBG(0000_0444 eax=0000_0110 ebx=0000_0111 ... edi=0000_0114)
  call snapshotRegisters
  PNT_NTSTR $dbgStartNtStr
  PNT_UX_PRETTY \v
  call dbgRegisters
  PNT_NTSTR $dbgEndNtStr
  call restoreRegisters
.endm

pntMem: # ( start end -- )
  movl (%ebp), %ecx  # start
  movl 4(%ebp), %edx  # end
  cmp %edx, %ecx
  jge pntMem_return
  movl (%ecx), %ebx
  call pntEbxUxPretty
  PNT_ASCII $' '
  addl $4, (%ebp)
  jmp pntMem
pntMem_return:
  addl $8, %ebp
  ret

pstackNtStr: .ascii "PSTACK: \0"
rstackNtStr: .ascii "RSTACK: depth=\0"
.align 4
pntPstack: # ( -- )
  PNT_NTSTR $pstackNtStr
  movl %ebp, -8(%ebp)       # start
  movl $pstackMax, -4(%ebp) # end
  subl $8, %ebp
  call pntMem
  ret

pntRstackDepth: # print the depth of the return stack
  PNT_NTSTR $rstackNtStr
  movl $rstackMax, %ebx
  subl %esp, %ebx
  subl $4, %ebx
  shrl $2, %ebx  # ebx=ebx/4
  PNT_UX_NL %ebx
  ret

.macro DBG_STACK v
  // Allows us to debug registers at varioius points
  call snapshotRegisters
  PNT_NTSTR $dbgStartNtStr
  PNT_UX_PRETTY \v
  PNT_ASCII $' '
  call pntPstack
  PNT_NTSTR $dbgEndNtStr
  call restoreRegisters
.endm

panicStartNtStr: .ascii "!! PANIC !! code=0x\0"
.align 4
panicDefault:
  PNT_NTSTR $panicStartNtStr
  PNT_UX_NL panicErr
  call pntPstack
  PNT_ASCII $'\n'
  call pntRstackDepth
  EXIT $RC_PANIC

.macro PANIC e
  movl \e, panicErr
  jmp *(panic)
.endm

checkErrno_str: .ascii "!! errno=\0"
.align 4
checkErrno:
  // Checks that errno is zero or panics
  // Mutates no registers.
  cmp $0, errno
  jz return
  PNT_NT_STR $checkErrno_str
  PNT_UX_NL errno
  PANIC $ERR_ERRNO

################################################################################
#### TESTING                                                                ####
################################################################################
// Testing is core to how this tutorial works. This contains utilities for
// testing our code.

testStartNtStr: .ascii "## Test \0"
testFailedNtStr: .ascii "!! Test failed: \0"
.align 4
startingTest:
  PNT_NT_STR $testStartNtStr
  PNT_NT_STR testingNtStr
  PNT_ASCII $'\n'
  ret

exitFailTest:
  PNT_NT_STR $testFailedNtStr
  PNT_NT_STR testingNtStr
  PNT_ASCII $'\n'
  call pntPstack
  PNT_ASCII $'\n'
  call pntRstackDepth
  EXIT $RC_TESTFAIL

assertFailStr: .ascii "!! ASSERT FAIL: \0"
assertEaxEbxEq_str: .ascii " = \0"
.align 4
assertEaxEbxEq:
  cmp %eax, %ebx
  je return
  PNT_NT_STR $assertFailStr
  movl %eax, %edi
  call pntEbxUx
  PNT_NT_STR $assertEaxEbxEq_str
  PNT_UX_NL %edi
  jmp exitFailTest

.macro SET_EABX a, b
  movl \a , %eax
  movl \b , %ebx
.endm

.macro ASSERT_EQ a, b
  SET_EABX \a, \b
  call assertEaxEbxEq
.endm

panicExpected:
  // Use this if a panic is expected. It will simply set panic=panicDefault
  // and jmp to panicRestore
  movl $panicDefault, panic
  jmp *(panicRestore)

.macro EXPECT_PANIC l
  movl \l, panicRestore
  movl $panicExpected, panic
.endm

panicErrInvalidStr: .ascii "!! panicErr not expected value: \0"
.align 4
panicErrInvalid:
  PNT_NT_STR $panicErrInvalidStr
  PNT_UX_NL panicErr
  jmp exitFailTest

.macro ASSERT_PANICED err
  // Assert that panicErr=err and then clear panicErr
  cmp \err, panicErr
  jne panicErrInvalid
  movl $0, panicErr
.endm

assertStackEmptyStr: .ascii "!! Stack not empty: \n\0"
.align 4
assertStackEmpty:
  movl $pstackMax, %eax
  subl %ebp, %eax
  jz return
  PNT_NT_STR $assertStackEmptyStr
  call pntPstack
  PNT_ASCII $'\n'
  call exitFailTest

assertPanicClean_str1: .ascii "!! Not clean panic==panicExpected\n\0"
assertPanicClean_str2: .ascii "!! Not clean panicErr==\0"
assertPanicClean:
  cmpl $panicExpected, panic
  je 1f
  cmpl $0, panicErr
  je return
  PNT_NT_STR $assertPanicClean_str2
  PNT_UX_NL panicErr
  call exitFailTest
1:
  PNT_NT_STR $assertPanicClean_str1
  call exitFailTest

assertClean:
  call checkErrno
  call assertPanicClean
  call assertStackEmpty
  ret

.macro TEST t
  testname_\t : .ascii "\t\0"
  .align 4
  test_\t :
    movl $testname_\t, testingNtStr
    call startingTest
    call assertClean
.endm

TEST suite
  call test_testing
  ret

TEST testing
  // Does an assert and an EXPECT_PANIC->PANIC
  ASSERT_EQ $0x42, $0x42
  EXPECT_PANIC $1f
  PANIC $0x42
  call unreachable
1:
  ASSERT_PANICED $0x42
  call assertClean
  ret


################################################################################
#### STACK                                                                  ####
################################################################################
// Our forth will primarily involve manipulating two stacks, the pstack
// (parameter stack) and rstack (return stack). We have defined pstackMin/Max
// and rstackMin/Max to specify the stack locations, and %ebp is our pstack
// pointer and %esp our rstack pointer.

pushDsInvalid:
  # Checks to make sure that the datastack hasn't overflowed.
  # If it has, re-increments and panics with appropriate error.
  addl $4, %ebp
  PANIC $ERR_PSTACK_OVERFLOW

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
  PANIC $ERR_PSTACK_UNDERFLOW

.macro DPOP reg
  movl (%ebp) , \reg
  cmpl $pstackMax, %ebp
  jge popDsInvalid
  addl $4, %ebp
.endm

##############################
# Main Entrypoint
mainStartNtStr: .ascii "Starting Triforth...\n\0"
.align 4
.globl _start
_start:
  movl $rstackMax, %esp  # clear rstack
  movl $pstackMax, %ebp  # clear pstack
  PNT_NT_STR $mainStartNtStr
  movl $0x110, %eax
  movl $0x111, %ebx
  movl $0x112, %ecx
  movl $0x113, %edx
  movl $0x114, %edi
  movl $0x115, %esi
  DBG $0x444
  call test_suite
  EXIT $0
