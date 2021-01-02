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


.equ FALSE, 0
.equ TRUE, 1

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
rstackExtraRoot: .space 0x4
rstackMinRoot: .space RSTACK_SIZE # return stack
rstackMaxRoot:
.equ PSTACK_SIZE, 0x100
pstackExtraRoot: .space 0x20      # guaranteed space above the pstack
pstackMinRoot: .space PSTACK_SIZE # data stack
pstackMaxRoot:
registerSnapshot: .space 0x20

pntFd: .int STDERR # where to PNT to
.equ EMIT_BUFFER_SIZE, 16
sourceFd: .int STDIN   # where the source code is read from
.equ WORD_MAX_CNT, 0x22
sourceEof: .byte 0 # Whether an EOF was encountered at the last read
sourceCStr: .byte 0 # Used for reading source word
sourceStr: .space WORD_MAX_CNT
errno: .int 0
panic: .int panicDefault # function to call when panicing
panicErr: .int 0  # error to print while panicing
rstackMin: .int 0
rstackMax: .int 0
pstackMin: .int 0
pstackMax: .int 0

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

.macro SET_AB a, b
  movl \a, %eax
  movl \b, %ebx
.endm
.macro SET_ABC a, b, c
  SET_AB \a, \b
  movl \c, %ecx
.endm
.macro SET_ABCD a, b, c, d
  SET_ABC \a, \b, \c
  movl \d, %edx
.endm
.macro SET_BC b, c
  movl \b, %ebx
  movl \c, %ecx
.endm

.macro JRZ reg , loc
  // Jump to location if the register is zero.  This uses `test`, which does an
  // AND that only mutates the eflags register.
  test \reg , \reg
  jz \loc
.endm

.macro JRNZ reg, loc
  // Jum to location if the register is not zero.
  test \reg, \reg
  jnz \loc
.endm

# cmp -> jmp is so common that we want to eliminate
# accidentally adding an instruction between them
# (which might alter the flags register).
.macro CMP_JMP cmpType, a, b, jmpCompare, location
  \cmpType \a, \b
  \jmpCompare \location
.endm

.macro JMP_E a, b, location
  CMP_JMP cmp, \a, \b, je, \location
.endm
.macro JMP_NE a, b, location
  CMP_JMP cmp, \a, \b, jne, \location
.endm

# Signed comparisons
.macro JMP_L a, b, location
  CMP_JMP cmp, \a, \b, jl, \location
.endm
.macro JMP_LE a, b, location
  CMP_JMP cmp, \a, \b, jle, \location
.endm
.macro JMP_GE a, b, location
  CMP_JMP cmp, \a, \b, jge, \location
.endm
.macro JMP_G a, b, location
  CMP_JMP cmp, \a, \b, jg, \location
.endm

# Unsigned comparisons
.macro JMP_B a, b, location # jmp below
  CMP_JMP cmp, \a, \b, jb, \location
.endm
.macro JMP_BE a, b, location # jmp below or equal
  CMP_JMP cmp, \a, \b, jbe, \location
.endm
.macro JMP_AE a, b, location # jmp above or equal
  CMP_JMP cmp, \a, \b, jae, \location
.endm
.macro JMP_A a, b, location # jmp above
  CMP_JMP cmp, \a, \b, ja, \location
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

.macro TODO
  EXIT $420
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
  JMP_NE $0, errno, return
  call syswrite
  JMP_L $0, %eax, syswriteall_error
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
          .ascii "pqrstuvwxyz{|}~\0"    # 0x70-7E
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

toasciiEbx: # ( u -- c )
  // Convert a number (typically 1-16) into ascii equivalent
  cmpl $9, %ebx
  jg toasciiEbx_gt9
  # number <= 9
  addl $'0', %ebx
  ret
toasciiEbx_gt9:
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
  movl $8, %eax     # %eax=index to store character
  movl %ebx, %ecx   # %ecx=remaining number
ebxUx_loop:
  dec %eax
  JMP_A $7, %eax, asmInvariantViolated  # ja=jump above (unsigned)
  movl %ecx, %ebx
  andl $0x0F, %ebx  # ebx=ebx % 0x10
  call toasciiEbx
  movb %bl, (%ebp, %eax, 1)
  shrl $4, %ecx    # ecx=ecx / 0x10
  jnz ebxUx_loop
  movl $8, %edx
  subl %eax, %edx # return count
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
  JMP_LE %ebp, %eax, return
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
  JMP_GE %edx, %ecx, pntMem_return
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
  movl %ebp, %ebx   # cache ebp (pstack pointer)
  subl $8, %ebp     # make space on stack
  movl %ebx, (%ebp)     # start
  movl pstackMax, %ebx
  movl %ebx, 4(%ebp)    # end
  call pntMem
  ret

pntRstackDepth: # print the depth of the return stack
  PNT_NTSTR $rstackNtStr
  movl rstackMax, %ebx
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

.macro PANIC_LANDING err, code
  .equ ERR_\err, \code
panicNow_\err :
  movl $ERR_\err, panicErr
  jmp *(panic)
.endm

.macro PANIC err
  jmp panicNow_\err
.endm

// Our Panic Locations

PANIC_LANDING SEE_MSG,          0x001
PANIC_LANDING OKAY,             0xBedabb1e
PANIC_LANDING PSTACK_OVERFLOW,  0x301
PANIC_LANDING PSTACK_UNDERFLOW, 0x303
PANIC_LANDING RSTACK_OVERFLOW,  0x304
PANIC_LANDING RSTACK_UNDERFLOW, 0x305
PANIC_LANDING MAX_WORD_LEN,     0x306
PANIC_LANDING WORD_MISSING,     0x307
PANIC_LANDING NUMBER_INVALID,   0x308
PANIC_LANDING UNKNOWN_WORD,     0x309
PANIC_LANDING STATE_REDUNDANT,  0x310
PANIC_LANDING STACK_UNDERFLOW,  0x311
PANIC_LANDING WORD_NOT_FOUND,   0x312
PANIC_LANDING INVALID_FLAG,     0x313
PANIC_LANDING INVALID_CHAR,     0x314
PANIC_LANDING STACK_NOT_EMPTY,  0x315
PANIC_LANDING STATE_INCORRECT,  0x316
PANIC_LANDING EMIT_ERR,         0x317
PANIC_LANDING NULL_NAMETOKEN,   0x318
PANIC_LANDING ERRNO,            0xDEADBEA7

panicNow_eaxIsErrno:
  movl %eax, errno
  jmp panicNow_ERRNO
  movl $ERR_ERRNO, panicErr
  jmp *(panic)

checkErrno_str: .ascii "!! errno=\0"
.align 4
checkErrno:
  // Checks that errno is zero or panics
  // Mutates no registers.
  JMP_E $0, errno, return
  PNT_NT_STR $checkErrno_str
  PNT_UX_NL errno
  jmp panicNow_ERRNO

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
  JMP_E %eax, %ebx, return
  subl $8, %ebp
  movl %eax, (%ebp)
  movl %ebx, 4(%ebp)
  PNT_NT_STR $assertFailStr
  PNT_UX (%ebp)
  PNT_NT_STR $assertEaxEbxEq_str
  PNT_UX_NL 4(%ebp)
  addl $8, %ebp
  jmp exitFailTest

.macro SET_EABX a, b
  movl \a , %eax
  movl \b , %ebx
.endm

.macro ASSERT_REG_EQ a, b
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
  cmpl \err, panicErr
  jne panicErrInvalid
  movl $0, panicErr
.endm

assertStackEmptyStr: .ascii "!! Stack not empty: \n\0"
.align 4
assertStackEmpty:
  movl pstackMax, %eax
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

.macro TESTCASE name
  testname_\name : .ascii "\name\0"
  .align 4
  test_\name :
    movl $testname_\name, testingNtStr
    call startingTest
    call assertClean
.endm

TESTCASE suite
  call test_pureAsm
  call test_pstack
  call test_bcmp
  call test_beqNocase
  call test_bmove
  call assertClean
  ret

TESTCASE pureAsm
  // Test simple assert
  ASSERT_REG_EQ $0x42, $0x42

  // Test panic/expect
  EXPECT_PANIC $1f
  PANIC OKAY
  call unreachable
1:ASSERT_PANICED $ERR_OKAY
  call assertClean

  // test countnt
  movl $asciiStr, %ecx
  call countntEcx
  ASSERT_REG_EQ $0x7F, %edx

  // test toasciiEbx
  .macro TEST_TOASCII v, e
    movl \v, %ebx
    call toasciiEbx
    ASSERT_REG_EQ \e, %ebx
  .endm
  TEST_TOASCII $0, $0x30
  TEST_TOASCII $1, $0x31
  TEST_TOASCII $9, $0x39
  TEST_TOASCII $0xA, $0x41
  TEST_TOASCII $0xF, $0x46

  // test ebxUx and ebxUxPad
  subl $8, %ebp # reserve stack space

  addl $8, %ebp # unreserve stack space

  // Explicit test that stack is empty
  ASSERT_REG_EQ %ebp, pstackMax
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
  PANIC PSTACK_OVERFLOW

.macro PUSH reg
  subl $4, %ebp
  cmpl pstackMin, %ebp
  jl pushDsInvalid
  movl \reg , (%ebp)
.endm

popDsInvalid:
  # Checks to make sure that the datastack hasn't underflowed.
  # If it has, re-decrements and panics with appropriate error.
  PANIC PSTACK_UNDERFLOW

.macro POP reg
  movl (%ebp) , \reg
  cmpl pstackMax, %ebp
  jge popDsInvalid
  addl $4, %ebp
.endm

drop:
  cmpl pstackMax, %ebp
  jge popDsInvalid
  addl $4, %ebp
  ret

assertEq: # ( a b -- )
  POP %eax
  POP %ebx
  call assertEaxEbxEq
  ret

.macro ASSERT_TOP v
  PUSH \v
  call assertEq
.endm

TESTCASE pstack
  PUSH $0x42
  PUSH $0x4422
  ASSERT_TOP $0x4422
  ASSERT_TOP $0x42
  EXPECT_PANIC $1f
  POP %eax # pop from empty stack
  call unreachable
1:ASSERT_PANICED $ERR_PSTACK_UNDERFLOW
  call assertStackEmpty

  movl pstackMin, %ebp # mimick full stack
  EXPECT_PANIC $1f
  PUSH $0x42
  call unreachable
1:ASSERT_PANICED $ERR_PSTACK_OVERFLOW
  movl pstackMax, %ebp
  call assertClean
  ret

pushRsInvalid:
  # Checks to make sure that the datastack hasn't overflowed.
  # If it has, re-increments and panics with appropriate error.
  addl $4, %esp
  PANIC RSTACK_OVERFLOW

.macro RPUSH reg
  pushl \reg
  cmpl rstackMin, %esp
  jl pushRsInvalid
.endm

popRsInvalid:
  # Checks to make sure that the datastack hasn't underflowed.
  # If it has, re-decrements and panics with appropriate error.
  subl $4, %esp
  PANIC RSTACK_UNDERFLOW

.macro RPOP reg
  popl \reg
  cmpl rstackMax, %esp
  jge popRsInvalid
.endm

################################################################################
#### Byte comparison functions                                              ####
################################################################################

.equ LT, -1
.equ EQ, 0
.equ GT, 1

bcmp: # %eax:aCount %ebx:&a %ecx:bCount %edx:&b ( -- ) %eax:cmp
  // Compares bytes at address &a to &b. Returns:
  // eax=-1 if a<b
  // eax=0 if a==b
  // eax=1 if a>b
  // "a is less than b " is defined as `a` having a byte with a lesser ascii
  // value OR (if no such byte is found) fewer count.
  // Uses: %edi
  PUSH %eax
  ZERO %edi
bcmp_loop:
  JMP_GE (%ebp), %edi, bcmp_aCountExhausted
  JMP_GE %ecx, %edi, bcmp_gt # jmp if b was exhausted, a wasn't, therefore a>b
  movb (%edi, %ebx, 1), %al
  cmpb (%edi, %edx, 1), %al # cmp b, a
  jl bcmp_lt
  jg bcmp_gt
  inc %edi
  jmp bcmp_loop
bcmp_aCountExhausted:
  JMP_GE %ecx, %edi, bcmp_eq # eq if bCount was also exhausted
  jmp bcmp_lt # else a<b
bcmp_eq:
  ZERO %eax # EQ
  jmp drop
bcmp_gt:
  movl $GT, %eax
  jmp drop
bcmp_lt:
  movl $LT, %eax
  jmp drop

.macro ASSERT_BYTES_CMP cmp, aCount, a, bCount, b
  SET_ABCD \aCount, \a, \bCount, \b
  call bcmp
  ASSERT_REG_EQ %eax, \cmp
.endm

test_bcmpStr: .ascii "aabbcc\0"
.align 4
TESTCASE bcmp
  ASSERT_BYTES_CMP $EQ, $3, $test_bcmpStr, $3, $test_bcmpStr   # same string is equal
  ASSERT_BYTES_CMP $LT, $2, $test_bcmpStr, $3, $test_bcmpStr   # less count is less than
  ASSERT_BYTES_CMP $LT, $5, $test_bcmpStr, $3, $test_bcmpStr+1 # a<b
  ASSERT_BYTES_CMP $GT, $2, $test_bcmpStr+4, $3, $test_bcmpStr # c>a
  ret

.macro TO_UPPER v
  # Convert ascii byte from lower->upper case.
  # Notes: 'A'=0x41 'Z'=0x5A 'a'=0x61 'z'=0x7A
  cmpb $'a', \v
  jl return
  cmpb $'z', \v
  jg return
  # else subtract 0x20 to convert to upper case.
  subb $0x20, \v
  ret
.endm

alToUpper: TO_UPPER %al
ahToUpper: TO_UPPER %ah

beqNocase: # %eax:aCount %ebx:&a %ecx:bCount %edx:&b ( -- ) %eax:bool
  // Compare byte sequence a to b for equality, ignoring case
  JMP_NE %eax, %ecx, beqNocase_neq
  ZERO %edi
beqNocase_loop:
  JMP_GE %ecx, %edi, beqNocase_eq
  movb (%edi, %ebx, 1), %al
  movb (%edi, %edx, 1), %ah # cmp b, a
  call alToUpper
  call ahToUpper
  inc %edi
  cmpb %al, %ah
  je beqNocase_loop # if eq continue loop, else not equal
beqNocase_neq:
  movl $FALSE, %eax
  ret
beqNocase_eq:
  movl $TRUE, %eax
  ret

.macro ASSERT_BYTES_NOCASE_EQ aCount, a, bCount, b
  SET_ABCD \aCount, \a, \bCount, \b
  call beqNocase
  ASSERT_REG_EQ %eax, $TRUE
.endm

.macro ASSERT_BYTES_NOCASE_NE aCount, a, bCount, b
  SET_ABCD \aCount, \a, \bCount, \b
  call beqNocase
  ASSERT_REG_EQ %eax, $FALSE
.endm

test_beqNocaseStr: .ascii "aaAAbbBB\0"
.align 4
TESTCASE beqNocase
  ASSERT_BYTES_NOCASE_EQ $3, $test_beqNocaseStr, $3, $test_beqNocaseStr   # same string =
  ASSERT_BYTES_NOCASE_EQ $2, $test_beqNocaseStr, $2, $test_beqNocaseStr+2 # different case =
  ASSERT_BYTES_NOCASE_NE $3, $test_beqNocaseStr, $2, $test_beqNocaseStr   # different lengths !=
  ASSERT_BYTES_NOCASE_NE $2, $test_beqNocaseStr, $2, $test_beqNocaseStr+3 # different letter !=
  ret

.macro ASSERT_STRCNT_NOCASE_EQ a, b
  # a and b are the addresses to counted bytes (bytes where first byte=count)
  movl \a, %ebx
  ZERO %eax
  movb (%ebx), %al  # aCount
  inc %ebx          # &a
  movl \b, %edx
  ZERO %ecx
  movb (%edx), %cl  # bCount
  inc %edx          # &b
  call beqNocase
  ASSERT_REG_EQ %eax, $TRUE
.endm

bmove: # %eax:count %ebx:&from %ecx:&to ( -- )
  // Moves count bytes from &from to &to starting at the beginning.
  // Uses registers a-d
  JRZ %eax, return
  movb (%ebx), %dl
  movb %dl, (%ecx)
  dec %eax
  inc %ebx
  inc %ecx
  jmp bmove

.macro BMOVE count, from, to
  SET_ABC \count, \from, \to
  call bmove
.endm

cStr_move: # %ebx:&fromCstr &ecx:&toCstr
  ZERO %eax
  movb (%ebx), %al   # get count of from
  movb %al, (%ecx)   # set `to` to same count
  inc %ebx
  inc %ecx
  call bmove
  ret

.macro CSTR_MOVE from, to
  SET_BC \from, \to
  call cStr_move
.endm

test_bmoveCStr: .ascii "/x06abcDEz"
test_bmoveCStr2: .ascii "/x03p12"
TESTCASE bmove
  subl $0x20, %ebp
  CSTR_MOVE $test_bmoveCStr, %ebp
  ASSERT_STRCNT_NOCASE_EQ $test_bmoveCStr, %ebp
  CSTR_MOVE $test_bmoveCStr2, %ebp
  ASSERT_STRCNT_NOCASE_EQ $test_bmoveCStr2, %ebp
  addl $0x20, %ebp
  ret

# sysopen: # %ebx:&fnameNt %ecx:intention %edx:permissions ( -- ) %eax:fd
#   # Open the nt filename.
#   # check %eax for errors and panic if they exist.
#   movl $SYS_OPEN, %eax
#   int $LINUX_SYSCALL
#   cmpl $0, %eax
#   jl 1f
#   ret
# 1:
#   movl %eax, panicErr
#   call *(panic)
#   jmp unreachable

.macro OPEN_SOURCE_FD filepathNt
.endm

.macro CLOSE_SOURCE_FD
.endm

sysread: # %ebx:fd %ecx:&buff %edx:maxLength ( -- ) %eax:numRead
  # Read from a filedescriptor. This does NOT panic on failure, the caller must
  # check %eax for errors.
  movl $SYS_READ, %eax
  int $LINUX_SYSCALL
  ret

findWhitespaceStart: # %eax:count %ebx:&bytes ( -- ) %eax:count_remain %ebx:&whitespace
  # Find whitespace. %ebx will be zero if there was none, else it will be a pointer
  # to the whitespace.
  JRZ %eax, findWhitespaceStart_none
  // all bytes > ascii-space are not whitespace. This means utf8 is supported
  // out of the box, since all non-ascii utf8 bytes are > 127
  cmpb $' ', (%ebx)
  jbe return # be="Below or Equal", unsigned variant of le
  dec %eax
  inc %ebx
  jmp findWhitespaceStart
findWhitespaceStart_none:
  movl $0, %ebx
  ret

findWhitespaceEnd: # %eax:count %ebx:&bytes ( -- ) %eax:count_remain %ebx:&not_whitespace
  JRZ %eax, findWhitespaceEnd_none
  cmpb $' ', (%ebx)
  ja return # ja=Jmp Above
  dec %eax
  inc %ebx
  jmp findWhitespaceEnd
findWhitespaceEnd_none:
  movl $0, %ebx
  ret

readSource:
  # Attempt to fill the sourceStr buffer.
  # Modifies sourceEof, sourceCStr, sourceStr with the result
  ZERO %ecx
  movb sourceCStr, %cl
  leal sourceStr(%ecx), %ecx # ecx=&buff
  # edx=maxCnt=MAX_CNT-strCnt
  movl $WORD_MAX_CNT, %edx
  subb sourceCStr, %dl  # edx=countToREad
  movl sourceFd, %ebx   # ebx=Fd
  call sysread
  JMP_L $0, %eax, panicNow_eaxIsErrno
  TODO
  addb %al, sourceCStr # add number read to the count
  cmp $0, sourceCStr


word: # ( b[0x24]:outCStr -- b[0x24]:outCStr)
  # Store the next word from the source into (%ebp).
  # The word can be up to x22 characters long.

  # Read up to the maxCnt bytes into sourceStr (keeping previous bytes that exist)
  # ecx=sourceStr+sourceCStr

  movb $0, (%ebp) # prime strCnt for cnt=0 if we return early
  JRNZ %al, word_notEof
# -- START EOF, al=0 is EOF
  JMP_E $0, sourceCStr, return # if sourceCStr=0 we have no word
  TODO # otherwise return the word we have
# -- END EOF
word_notEof:
  movb sourceCStr, %al # and get it
  movl sourceStr, %ebx   # %ebx=&bytes
  call findWhitespaceStart # eax=count_remain %ebx=&whitespace
  JRZ %ebx, word_noWhitespace

  # Store count of word in the return strCnt byte
  movl sourceCStr, %ecx
  subl %eax, %ecx
  movb %cl, (%ebp)
  # Store what will be the new count into sourceCStr
  movb %al, sourceCStr

  # Move word from sourceCStr -> strCnt
  movb (%ebp), %al # count
  movl sourceStr, %ebx # &from
  leal 1(%ebp), %ecx # &to
  call bmove

  # Find end of whitespace
  movl sourceStr, %ebx
  movb (%ebp), %al    # strCnt
  addl %eax, %ebx # ebx=&bytes after word
  movb sourceCStr, %al
  subb (%ebp), %al # al=remaining count in sourceStr
  call findWhitespaceEnd

  # Move non-whitespace to start of sourceStr
  movb %al, sourceCStr # update count
  movl sourceStr, %ecx # ecx=&to, ebx already is from
  call bmove
  ret
word_noWhitespace:
  cmpb $WORD_MAX_CNT, sourceCStr
  jge panicNow_MAX_WORD_LEN
  jmp word # try again

##############################
# Main Entrypoint
mainStartNtStr: .ascii "Starting Triforth...\n\0"
.align 4
.globl _start
_start:
  movl $rstackMinRoot, rstackMin
  movl $rstackMaxRoot, rstackMax
  movl $pstackMinRoot, pstackMin
  movl $pstackMaxRoot, pstackMax
  movl rstackMax, %esp  # clear rstack
  movl pstackMax, %ebp  # clear pstack
  PNT_NT_STR $mainStartNtStr
  call test_suite
  EXIT $0
