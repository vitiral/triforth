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
.macro SET_BCD b, c, d
  SET_BC \b, \c
  movl \d, %edx
.endm

# These set byte values
.macro MOVB_A value
  ZERO %eax
  movb \value, %al
.endm
.macro MOVB_B value
  ZERO %ebx
  movb \value, %bl
.endm
.macro MOVB_C value
  ZERO %ecx
  movb \value, %cl
.endm
.macro MOVB_D value
  ZERO %edx
  movb \value, %dl
.endm

.macro JRZ reg , loc
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
.macro MUL4 a
  shll $2, \a # x<<2 is same as x*2^2=x*4
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
  movl (registerSnapshot+offset_\r) , %\r
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
  // uses: ecx, edx
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

.macro PNT_NTSTR_NL s
  PNT_NTSTR \s
  PNT_ASCII $'\n'
.endm

pntCStr: # %ecx=&cStr
  movl pntFd, %ebx
  MOVB_D (%ecx)
  inc %ecx
  call syswriteall
  ret

.macro PNT_CSTR cStr
  movl \cStr, %ecx
  call pntCStr
.endm

.macro PNT_CSTR_NL cStr
  PNT_CSTR \cStr
  PNT_ASCII $'\n'
.endm

todoNtStr: .ascii "!! TODO reached !!\n\0"
.align 4
.macro TODO
  PNT_NTSTR $todoNtStr
  EXIT $42
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
.macro DBG_REG reg
  PNT_NTSTR $dbgRegNtStr_\reg
  PNT_UX_PRETTY registerSnapshot+offset_\reg
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
PANIC_LANDING MAX_WORD_CNT,     0x306
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
  PNT_NTSTR $checkErrno_str
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
  PNT_NTSTR $testStartNtStr
  PNT_NTSTR testingNtStr
  PNT_ASCII $'\n'
  ret

exitFailTest:
  PNT_NTSTR $testFailedNtStr
  PNT_NTSTR testingNtStr
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
  PNT_NTSTR $assertFailStr
  PNT_UX (%ebp)
  PNT_NTSTR $assertEaxEbxEq_str
  PNT_UX_NL 4(%ebp)
  addl $8, %ebp
  jmp exitFailTest

assertEaxEbxNe_str: .ascii " = \0"
.align 4
assertEaxEbxNe:
  JMP_NE %eax, %ebx, return
  subl $8, %ebp
  movl %eax, (%ebp)
  movl %ebx, 4(%ebp)
  PNT_NTSTR $assertFailStr
  PNT_UX (%ebp)
  PNT_NTSTR $assertEaxEbxNe_str
  PNT_UX_NL 4(%ebp)
  addl $8, %ebp
  jmp exitFailTest

.macro ASSERT_REG_EQ a, b
  SET_AB \a, \b
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
  PNT_NTSTR $panicErrInvalidStr
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
  PNT_NTSTR $assertStackEmptyStr
  call pntPstack
  PNT_ASCII $'\n'
  call exitFailTest

assertPanicClean_str1: .ascii "!! Not clean panic==panicExpected\n\0"
assertPanicClean_str2: .ascii "!! Not clean panicErr==\0"
.align 4
assertPanicClean:
  cmpl $panicExpected, panic
  je 1f
  cmpl $0, panicErr
  je return
  PNT_NTSTR $assertPanicClean_str2
  PNT_UX_NL panicErr
  call exitFailTest
1:
  PNT_NTSTR $assertPanicClean_str1
  call exitFailTest

assertClean:
  call checkErrno
  call assertPanicClean
  call assertStackEmpty
  ret

.macro TESTCASE name
  testname_\name : .ascii "\name\0"
  .align 4
  testcase_\name :
    movl $testname_\name, testingNtStr
    call startingTest
    call assertClean
  testStart_\name :
.endm

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

.macro PUSHP reg
  subl $4, %ebp
  cmpl pstackMin, %ebp
  jl pushDsInvalid
  movl \reg , (%ebp)
.endm

popDsInvalid:
  PANIC PSTACK_UNDERFLOW

.macro POPP reg
  movl (%ebp) , \reg
  cmpl pstackMax, %ebp
  jge popDsInvalid
  addl $4, %ebp
.endm


.macro DROP_INVALID_LANDING n
dropInvalid\n :
  subl $4*\n , %ebp
  PANIC PSTACK_UNDERFLOW
.endm

DROP_INVALID_LANDING 1
DROP_INVALID_LANDING 2
DROP_INVALID_LANDING 3
DROP_INVALID_LANDING 4

.macro DROPP n
  addl $4*\n , %ebp
  cmpl pstackMax, %ebp
  jg dropInvalid\n
.endm

drop:
  cmpl pstackMax, %ebp
  jge popDsInvalid
  addl $4, %ebp
  ret

assertEq: # ( a b -- )
  POPP %eax
  POPP %ebx
  call assertEaxEbxEq
  ret

.macro ASSERT_TOP v
  PUSHP \v
  call assertEq
.endm

TESTCASE pstack
  PUSHP $0x42
  PUSHP $0x4422
  ASSERT_TOP $0x4422
  ASSERT_TOP $0x42
  EXPECT_PANIC $1f
  POPP %eax # pop from empty stack
  call unreachable
1:ASSERT_PANICED $ERR_PSTACK_UNDERFLOW
  call assertStackEmpty

  movl pstackMin, %ebp # mimick full stack
  EXPECT_PANIC $1f
  PUSHP $0x42
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

.macro PUSHR reg
  pushl \reg
  cmpl rstackMin, %esp
  jl pushRsInvalid
.endm

popRsInvalid:
  # Checks to make sure that the datastack hasn't underflowed.
  # If it has, re-decrements and panics with appropriate error.
  subl $4, %esp
  PANIC RSTACK_UNDERFLOW

.macro POPR reg
  popl \reg
  cmpl rstackMax, %esp
  jge popRsInvalid
.endm

.macro PUSHP2 a, b  # ( -- a b )
  PUSHP \b
  PUSHP \a
.endm
.macro PUSHP3 a, b, c # ( -- a b c )
  PUSHP \c
  PUSHP2 \a, \b
.endm
.macro PUSHP4 a, b, c, d # ( -- a b c d )
  PUSHP \d
  PUSHP3 \a, \b, \c
.endm
.macro PUSHP5 a, b, c, d, e # ( -- a b c d e )
  PUSHP \e
  PUSHP4 \a, \b, \c, \d
.endm
.macro GET_P index, reg
  movl 4*\index(%ebp), \reg
.endm
.macro SET_P index, reg
  movl \reg, 4*\index(%ebp)
.endm

################################################################################
#### Byte comparison functions                                              ####
################################################################################

.equ LT, -1
.equ EQ, 0
.equ GT, 1

bcmp: # %eax:&a %ebx:aCount %ecx:&b %edx:bCount ( -- ) %eax:cmp
  // Compares bytes at address &a to &b. Returns:
  // eax=-1 if a<b
  // eax=0 if a==b
  // eax=1 if a>b
  // "a is less than b " is defined as `a` having a byte with a lesser ascii
  // value OR (if no such byte is found) fewer count.
  // Uses: %edi
  PUSHP %ebx
  ZERO %edi
bcmp_loop:
  JMP_GE (%ebp), %edi, bcmp_aCountExhausted
  JMP_GE %edx, %edi, bcmp_gt # jmp if b was exhausted, a wasn't, therefore a>b
  movb (%eax,%edi,1), %bl
  cmpb (%ecx,%edi,1), %bl # cmp b, a
  jl bcmp_lt
  jg bcmp_gt
  inc %edi
  jmp bcmp_loop
bcmp_aCountExhausted:
  JMP_GE %edx, %edi, bcmp_eq # eq if bCount was also exhausted
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

.macro ASSERT_BYTES_CMP cmp, a, aCount, b, bCount
  SET_ABCD \a, \aCount, \b, \bCount
  call bcmp
  ASSERT_REG_EQ %eax, \cmp
.endm

test_bcmpStr: .ascii "aabbcc\0"
.align 4
TESTCASE bcmp
  ASSERT_BYTES_CMP $EQ, $test_bcmpStr,   $3, $test_bcmpStr   ,$3 # same string is equal
  ASSERT_BYTES_CMP $LT, $test_bcmpStr,   $2, $test_bcmpStr   ,$3 # less count is less than
  ASSERT_BYTES_CMP $LT, $test_bcmpStr,   $5, $test_bcmpStr+1 ,$3 # a<b
  ASSERT_BYTES_CMP $GT, $test_bcmpStr+4, $2, $test_bcmpStr   ,$3 # c>a
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

beqNocase: # %eax:&a %ebx:aCount %ecx:&b %edx:bCount ( -- ) %eax:bool
  // Compare byte sequence a to b for equality, ignoring case
  JMP_NE %ebx, %edx, beqNocase_neq
  movl %eax, %ebx  # ebx:&a
  ZERO %edi
beqNocase_loop:
  JMP_GE %edx, %edi, beqNocase_eq
  movb (%edi, %ebx, 1), %al
  movb (%edi, %ecx, 1), %ah
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
  SET_ABCD \a, \aCount, \b, \bCount
  call beqNocase
  ASSERT_REG_EQ %eax, $TRUE
.endm

.macro ASSERT_BYTES_NOCASE_NE aCount, a, bCount, b
  SET_ABCD \a, \aCount, \b, \bCount
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

cStrMove: # %ebx:&fromCstr &ecx:&toCstr
  MOVB_A (%ebx)      # eax = count of from
  movb %al, (%ecx)   # set `to` to same count
  inc %ebx  # inc both to point at bytes (not count)
  inc %ecx
  call bmove
  ret

.macro CSTR_MOVE from, to
  SET_BC \from, \to
  call cStrMove
.endm

cmpCStr: # %eax:&aCStr %ecx:&bCStr ( -- ) %eax:cmp
  MOVB_B (%eax)
  inc %eax
  MOVB_D (%ecx)
  inc %ecx
  call bcmp
  ret

cStrNocaseEq: # %eax:&aCStr %ecx:&bCStr
  MOVB_B (%eax)
  inc %eax
  MOVB_D (%ecx)
  inc %ecx
  call beqNocase
  ret

.macro ASSERT_CSTR_NOCASE_EQ aCStr, bCStr
  movl \aCStr, %eax
  movl \bCStr, %ecx
  call cStrNocaseEq
  ASSERT_REG_EQ %eax, $TRUE
.endm

test_bmoveCStr: .byte 0x06
  .ascii "abcDEz"
test_bmoveCStr2: .byte 0x03
  .ascii "p12"
.align 4
TESTCASE bmove
  subl $0x20, %ebp
rightHereTODO:
  CSTR_MOVE $test_bmoveCStr, %ebp
  ASSERT_CSTR_NOCASE_EQ $test_bmoveCStr, %ebp
  CSTR_MOVE $test_bmoveCStr2, %ebp
  ASSERT_CSTR_NOCASE_EQ $test_bmoveCStr2, %ebp
  addl $0x20, %ebp
  ret

zeroFyte: # %eax:count %ebx:&fyte
  // Zero memory in fyte sizes
  JRZ %eax, return
  dec %eax
  movl $0, (%ebx, %eax, 4)
  jmp zeroFyte

################################################################################
#### Simple Pure Assembly Utility Functions and Macros                      ####
################################################################################
.macro ALIGN4 reg
  // modify reg so it is aligned by 4
  addl $3, \reg   # Add 3 to the number, pushing it up to or over 4byte boundary
                  # iff not aligned
  andl $~3, \reg  # Same as %4 (but faster). Gets rid of two lowest bytes.
.endm

.macro TEST_ALIGN4 before, expected
  movl \before, %eax
  ALIGN4 %eax
  movl \expected, %ebx
  call assertEaxEbxEq
.endm
TESTCASE align4
  TEST_ALIGN4 $0, $0
  TEST_ALIGN4 $1, $4
  TEST_ALIGN4 $2, $4
  TEST_ALIGN4 $3, $4
  TEST_ALIGN4 $4, $4
  TEST_ALIGN4 $0x4000, $0x4000
  TEST_ALIGN4 $0x4001, $0x4004
  TEST_ALIGN4 $0x4004, $0x4004
  ret

sllPush: # %eax:&node (or &root) %ebx:&newNode ( -- )
  // Singly-linked-list: push newNode to be after node
  // uses: eax, ebx, ecx
  // Before: node -> nextNode
  // After:  node -> newNode -> nextNode
  movl (%eax), %ecx  # ecx=node.&next
  movl %ecx, (%ebx)  # newNode.&next=node.&next
  movl %ebx, (%eax)  # node.&next=&newNode
  ret

sllPop: # %eax:&prev &ebx:&node
  // pop node
  // Before: prev -> node -> nextNode
  // After: prev -> nextNode
  movl (%ebx), %ecx  # ecx=node.&next
  movl %ecx, (%eax)  # set prev.&next = node.&next
  movl $0, (%ebx)    # clear node.&next
  ret

.macro SLL_PUSH node, newNode
  SET_AB \node, \newNode
  call sllPush # a -> b
.endm
.macro SLL_POP prevNode, node
  SET_AB \prevNode, \node
  call sllPop
.endm

TESTCASE sll
  movl $0, testSpace_00 # 00 (&root) starts at null
  movl $0xDeadBeef, testSpace_04 # 04 starts at something else
  SLL_PUSH $testSpace_00, $testSpace_04
  ASSERT_REG_EQ (testSpace_00), $testSpace_04
  ASSERT_REG_EQ (testSpace_04), $0
  SLL_PUSH $testSpace_00, $testSpace_08
  ASSERT_REG_EQ (testSpace_00), $testSpace_08
  ASSERT_REG_EQ (testSpace_08), $testSpace_04
  ASSERT_REG_EQ (testSpace_04), $0

  SLL_POP $testSpace_00, $testSpace_08
  ASSERT_REG_EQ (testSpace_08), $0
  ASSERT_REG_EQ testSpace_00, $testSpace_04
  ASSERT_REG_EQ testSpace_04, $0
  ret


################################################################################
#### Reading words from source                                              ####
################################################################################

sysopen: # %ebx:&fnameNt %ecx:intention %edx:permissions ( -- ) %eax:fd
  # Open the nt filename.
  # check %eax for errors and panic if they exist.
  movl $SYS_OPEN, %eax
  int $LINUX_SYSCALL
  ret

sysclose: # %ebx:fd ( -- ) 
  # Close the filedescriptor
  movl $SYS_CLOSE, %eax
  int $LINUX_SYSCALL
  ret

.macro OPEN_SOURCE_FD filepathNt
  SET_BCD \filepathNt, $O_RDONLY, $0666 # edx=permissions
  call sysopen
  JMP_L $0, %eax, panicNow_eaxIsErrno
  movl %eax, sourceFd
.endm
.macro CLOSE_SOURCE_FD
  movl sourceFd, %ebx
  call sysclose
  JMP_L $0, %eax, panicNow_eaxIsErrno
  movl $STDIN, sourceFd
  movb $0, sourceEof  # reset source Eof
  movb $0, sourceCStr # reset source Count
.endm

sysread: # %ebx:fd %ecx:&buff %edx:maxLength ( -- ) %eax:numRead
  # eax=0 for EOF and negative for error
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

fillSourceCStr:
  # Attempt to fill the sourceStr buffer.
  # Modifies sourceEof, sourceCStr, sourceStr with the result
  MOVB_C sourceCStr     # count of sourceStr
  leal sourceStr(%ecx), %ecx # ecx=&buff, end of sourceStr space
  movl $SOURCE_STR_CNT_MAX, %edx
  subb sourceCStr, %dl  # edx=countToRead
  movl sourceFd, %ebx   # ebx=Fd
  call sysread
  JMP_L $0, %eax, panicNow_eaxIsErrno
  JRZ %eax, readSource_eof
  addb %al, sourceCStr # add number read to the count
  ret
readSource_eof:
  movb $TRUE, sourceEof
  ret

flush_sourceCStr_to_wordCStr:
  // %eax:count ( -- )
  // Does two things:
  // - Moves the word of length %eax into wordCStr
  // - Moves the remaining (non-whitespace) bytes to beginning of sourceCStr
  //   and updates count
  movb %al, wordCStr  # update wordCnt
  movl $sourceStr, %ebx # &from
  movl $wordStr, %ecx # &to
  call bmove # note: eax already=count

  MOVB_A sourceCStr  # eax will = count leftover in sourceCStr
  subb wordCStr, %al   # eax=sourceCount - wordCount
  MOVB_B wordCStr
  leal sourceStr(%ebx), %ebx # ebx=&start_of_whitespace
  call findWhitespaceEnd
  movb %al, sourceCStr  # update count
  # eax=count-after-non-whitespace, ebx=&after-non-whitespace
  movl $sourceStr, %ecx # ecx=&to
  call bmove
  ret

word: # Store the next word from the source into wordCStr
  call fillSourceCStr

  MOVB_A sourceCStr # eax=sourceCount
  JRZ %eax, word_noWhitespace # if count=0, check Eof and try again
  movl $sourceStr, %ebx
  call findWhitespaceStart # note: eax:count_remain %ebx:&whitespace
  JRZ %ebx, word_noWhitespace

  # Found a word, update wordCStr and sourceCStr and return
  movl %eax, %ebx # eax will = wordCount
  MOVB_A sourceCStr
  subl %ebx, %eax
  call flush_sourceCStr_to_wordCStr
  ret
word_noWhitespace:
  // There can be no whitespace for a few reasons:
  // - We reached the EOF, meaning this is the last word -- return whatever we
  //   have (may be empty) 
  // - We simply didn't read enough bytes, try again.
  // - The word was too large to fit in the buffer, this is an error.
  cmpb $0, sourceEof
  jne word_eof
  cmpb $WORD_CNT_MAX, sourceCStr
  // jbe: didn't read enough bytes, try again. Note, if equal may be
  // because we reached EOF, but we won't know until we try again.
  jbe word
  PANIC MAX_WORD_CNT
word_eof: # eof reached, return what we have
  MOVB_A sourceCStr
  call flush_sourceCStr_to_wordCStr
  ret

.macro TEST_FIND_WHITESPACE fn, cStr, expectedCountRemaining
  # Test findWhitespace using the cStr and the count
  # First, assert expectedCountRemaining is correct.
  MOVB_A \cStr
  movl $\cStr+1, %ebx
  call \fn
  movl %ebx, %ecx # cache ebx for next assert
  movl $\expectedCountRemaining, %ebx
  call assertEaxEbxEq

  # Next, assert the pointer points to the right memory
  movb \cStr, %al  # total count
  subl $\expectedCountRemaining, %eax # eax=size of whitespace
  addl $\cStr, %eax # eax=expected pointer location
  inc %eax # inc because of the count byte in cStr
  movl %ecx, %ebx # load cached ebx
  call assertEaxEbxEq
.endm

test_wordCStr1: .byte 0xC
test_wordStr1: .ascii "the    quick"
test_wordCStr2: .byte 8
test_wordStr2: .ascii "   quick"
.align 4
TESTCASE findWhitespace
  TEST_FIND_WHITESPACE findWhitespaceStart, test_wordCStr1, 9 # note: 9 REMAIN
  TEST_FIND_WHITESPACE findWhitespaceStart, test_wordCStr2, 8
  TEST_FIND_WHITESPACE findWhitespaceEnd, test_wordCStr1, 0xC
  TEST_FIND_WHITESPACE findWhitespaceEnd, test_wordCStr2, 5
  ret

file_some_words: .ascii "tests/some_words.txt\0"
file_some_words_cStr: .byte 0x46
  .ascii "the     quick\n\n\t\t    \tbrown\n  fox jumped over    the "
  .ascii "lazy    dog\n\nthe "
test_wordCStr_specialFill: .byte 0x46
  .ascii "the     quick\n\n\t\t    \tbrown\n  fox jumped over    the "
  .ascii "lazy    dog\n\n    "
.align 4
TESTCASE fillSourceCStr
  OPEN_SOURCE_FD $file_some_words
  call fillSourceCStr # do a normal fill, assert as expected
  ASSERT_CSTR_NOCASE_EQ $sourceCStr, $file_some_words_cStr
  # now fill only 4 characters
  movb $0x42, sourceCStr  # 0x46 - 4
  call fillSourceCStr # do a normal fill, assert as expected
  ASSERT_CSTR_NOCASE_EQ $sourceCStr, $test_wordCStr_specialFill
  CLOSE_SOURCE_FD
  ret

TESTCASE flush_sourceCStr_to_wordCStr
  CSTR_MOVE $test_wordCStr1, $sourceCStr
  movl $3, %eax
  call flush_sourceCStr_to_wordCStr # moves "the" to stack, keeps "quick"
  movl $wordStr, %ebx
  ASSERT_BYTES_CMP $EQ, %ebx, $3,$test_wordStr1, $3 # stack = "the"
  MOVB_A wordCStr # eax=wordCnt
  ASSERT_REG_EQ %eax, $3
  ASSERT_BYTES_CMP $EQ, $sourceStr, $5, $test_wordStr1+7, $5  # remaining source="quick"
  movb $0, sourceEof  # reset source Eof
  movb $0, sourceCStr # reset source Count
  ret

test_wordWordsNtStr:
  .ascii "the\0quick\0brown\0fox\0jumped\0over\0the\0lazy\0dog\0"
  .ascii "the\0quick\0brown\0fox\0jumped\0over\0the\0lazy\0dog\0\0"
.align 4
TESTCASE word
  PUSHR $test_wordWordsNtStr # (%esp)=expected word
  OPEN_SOURCE_FD $file_some_words
test_words:
  call word # get word from sourceFd
  # Get expected word
  movl (%esp), %ecx
  call countntEcx    # ecx=ntstr edx=count
  # Update expected word for next loop (%esp) = (%exp) + %edx + 1
  addl %edx, (%esp)
  addl $1, (%esp)
  # Assert expected = found
  MOVB_B wordCStr     # ebx=wordCnt
  movl $wordStr, %eax # eax=&word
  call bcmp
  ASSERT_REG_EQ %eax, $EQ
  JRNZ %edx, test_words  # if the word len != 0, loop again
test_words_end:
  CLOSE_SOURCE_FD
  POPR %eax
  ret

################################################################################
#### Forth Dictionary                                                       ####
################################################################################

# Offsets into the global "dict struct"
.equ DICT_OFFSET_ROOT,      0x0
.equ DICT_OFFSET_CODE_MIN,  0x0
.equ DICT_OFFSET_CODE_HERE, 0x4
.equ DICT_OFFSET_CODE_MAX,  0x8
.equ DICT_OFFSET_META_MIN,  0xC
.equ DICT_OFFSET_META_HERE, 0x10
.equ DICT_OFFSET_META_MAX,  0x14
.equ DEFAULT_DICT_BUCKETS_CNT, 0x1000

.macro DICT_GLOBAL_GET_ADDR field, dictGlobalAddr
  // Get a field from the dictionary (ROOT, [CODE|META]_[MIN|MAX|HERE])
  // dictGlobalAddr becomes the address of the field
  addl $DICT_OFFSET_\field, \dictGlobalAddr
.endm

.equ DICT_SIZE_WITHOUT_NAME, 3 * 4
.equ DICT_FIELD_nextAddr, 0x0
.equ DICT_FIELD_xt,       0x4
.equ DICT_FIELD_info0,    0x8
.equ DICT_FIELD_nameCStr, 0xC
.macro DICT_META_GET_ADDR metaAddr, field, reg
  movl \metaAddr, \reg
  addl $DICT_FIELD_\field, \reg
.endm
.macro DICT_META_GET metaAddr, field, reg
  DICT_META_GET_ADDR \metaAddr, \field, \reg
  movl (\reg), \reg
.endm
.macro DICT_META_SET metaAddr, field, value, reg
  # Set value to field. Use reg as temporary register.
  DICT_META_GET_ADDR \metaAddr, \field, \reg
  movl \value, (\reg)
.endm

.macro SETUP_DICT_CODE type, defaultSize
  movl heap, %eax
  movl %eax, dictCodeMin\type
  movl %eax, dictCodeHere\type
  addl \defaultSize, %eax
  movl %eax, dictCodeMax\type
  movl %eax, heap
.endm
.macro SETUP_DICT_META type, defaultSize
  movl heap, %eax
  movl %eax, dictMetaMin\type
  movl %eax, dictMetaHere\type
  addl \defaultSize, %eax
  movl %eax, dictMetaMax\type
  movl %eax, heap
.endm
.equ DEFAULT_DICT_SECTION_SIZE, 0x40000
setupDict:
  SETUP_DICT_CODE RTime,      $DEFAULT_DICT_SECTION_SIZE
  SETUP_DICT_CODE CTime,  $DEFAULT_DICT_SECTION_SIZE

  SETUP_DICT_META RTime,      $DEFAULT_DICT_SECTION_SIZE
  SETUP_DICT_META CTime,  $DEFAULT_DICT_SECTION_SIZE
  movl $DEFAULT_DICT_BUCKETS_CNT-1, dictBucketsAndValue
  movl heap, %eax
  movl %eax, dictBucketsAddr
  addl $DEFAULT_DICT_BUCKETS_CNT*4, heap
  movl $DEFAULT_DICT_BUCKETS_CNT, %eax
  movl dictBucketsAddr, %ebx
  call zeroFyte
  ret

pjwHashEax: # %ecx:&bytes %edx:count  ( -- ) %edi:hash
  // Do a PJW hash on count bytes.
  // https://en.wikipedia.org/wiki/PJW_hash_function
  ZERO %edi
  addl %ecx, %edx # edx=end addr
pjwHashEax_loop:
  JMP_AE %edx, %ecx, return
  # Next 4 lines: h = (h << 4) + *s++
  shll $4, %edi
  MOVB_A (%ecx)
  addl %eax, %edi
  inc %edi
  movl %edi, %ebx  # ebx will = high bits below
  andl $0xF0000000, %ebx
  JRZ %ebx, 1f # if(high)
  # Next three lines are: h ^= high >> 24
  movl %ebx, %eax # eax is used as temprary, need to keep high
  shrl $24, %eax
  xorl %eax, %edi
1:
  # Next two lines are: h &= ~high
  notl %ebx
  andl %ebx, %edi
  inc %ecx
  jmp pjwHashEax_loop

dictCStrBucketAddr: # %ecx:&cStr ( -- ) %edi:&root
  // Note: &root isn't _really_ a node (has no data) but acts like a node for
  // sll* methods since the first item is a pointer to a node.
  MOVB_D (%ecx)
  inc %ecx # ecx=&str
  call pjwHashEax
  andl dictBucketsAndValue, %edi
  MUL4 %edi
  addl dictBucketsAddr, %edi
  ret

.equ DICT_TYPE_RTIME, 0 # runtime
.equ DICT_TYPE_CTIME, 1 # compiletime
getDictTypeGlobalAddr: # %eax:dictType ( -- ) %eax:dictGlobalAddr
  JRZ %eax, dictAddr_RTime
  JMP_E $DICT_TYPE_CTIME, %eax, dictAddr_CTime
  jmp asmInvariantViolated
.macro dictAddr_ type
  dictAddr_\type :
    movl $dict\type , %eax
    ret
.endm
dictAddr_ RTime
dictAddr_ CTime

.align 4
dictPush: # %eax:dictType %ecx:&nameCStr ( -- ) %eax:mt (aka &newNode)
  PUSHP %ecx # stack: ( &nameCStr )
  call getDictTypeGlobalAddr
  DICT_GLOBAL_GET_ADDR META_HERE, %eax # %eax=&metaHere
  movl (%eax), %edx # edx=metaHere=&newNode
  # Calculate new metaHere via ecx=newNodeSize
  GET_P 3, %ebx # ebx=&wordCStr
  MOVB_C (%ebx) # ecx=wordCnt
  inc %ecx      # plus Cnt byte
  addl %edx, %ecx # plus original metaHere
  addl $DICT_SIZE_WITHOUT_NAME, %ecx # plus normal size of dict
  // TODO: check META_MAX
  movl %ecx, (%eax)
  // Set initial fields in new entry (xt, info0, nameCStr)
  DICT_META_SET %edx, xt, $0, %eax    # initial xt=0
  DICT_META_SET %edx, info0, $0, %eax # inital info0=0
  DICT_META_GET_ADDR %edx, nameCStr, %ecx  # %ecx=&toCStr
  POPP %ebx   # ebx=&nameCStr ecx=&toStr stack: ( )
  PUSHP %edx  # stack: ( &newNode )
  call cStrMove
  // Insert new node into dictionary
  GET_P 0, %edx # edx=&newNode
  DICT_META_GET_ADDR %edx, nameCStr, %ecx # %ecx=&nameCStr
  call dictCStrBucketAddr # edi=&root
  movl %edi, %eax # eax=&root
  GET_P 0, %ebx   # ebx=&newNode stack: ( )
  call sllPush
  POPP %eax # eax=&node=mt
  ret

metaSllFindName: # %eax:&node %ecx:&cStr ( -- ) %eax:&node
  // Find the name in the meta dictionary given the first node.
  PUSHP2 %eax, %ecx # ( &node, &cStr )
metaSllFindName_loop:
  JRZ %eax, metaSllFindName_dne
  addl $DICT_FIELD_nameCStr, %eax # eax=&nameCStr
  call cStrNocaseEq
  JMP_E $TRUE, %eax, metaSllFindName_found
  GET_P 0, %eax
  movl (%eax), %eax # eax=eax.&next
  SET_P 0, %eax # store new &node in pstack
  GET_P 1, %ecx # ecx:&cStr
  jmp metaSllFindName_loop
metaSllFindName_found:
  GET_P 0, %eax
metaSllFindName_dne:
  DROPP 2
  ret

dictFindName: # %ecx:&cStr ( -- ) &eax:&node
  // eax=0 if no node found.
  PUSHP %ecx
  call dictCStrBucketAddr
  JRZ %edi, return
  movl (%edi), %eax # eax=&firstNode
  POPP %ecx
  call metaSllFindName
  ret

######
# Dictionary tests
.macro TEST_HASH assert, aCStr, bCStr
  # asserts that a compares to b
  MOVB_D \aCStr
  movl $\aCStr+1, %ecx
  call pjwHashEax
  PUSHP %edi
  MOVB_D \bCStr
  movl $\bCStr+1, %ecx
  call pjwHashEax
  POPP %ebx
  movl %edi, %eax
  call \assert
.endm
hashTest_cStr1: .byte 1
  .ascii "a"
hashTest_cStr2: .byte 2
  .ascii "aa"
hashTest_cStr3: .byte 5
  .ascii "zzzzz"
hashTest_cStr4: .byte 5
  .ascii "zzazz"
.align 4
TESTCASE pjwHashEax
  // This isn't a very good test, but makes sure there is some spread in the
  // values.
  TEST_HASH assertEaxEbxEq, hashTest_cStr1, hashTest_cStr1
  TEST_HASH assertEaxEbxEq, hashTest_cStr2, hashTest_cStr2
  TEST_HASH assertEaxEbxEq, hashTest_cStr4, hashTest_cStr4
  TEST_HASH assertEaxEbxNe, hashTest_cStr1, hashTest_cStr2
  TEST_HASH assertEaxEbxNe, hashTest_cStr1, hashTest_cStr3
  TEST_HASH assertEaxEbxNe, hashTest_cStr3, hashTest_cStr4
  ret

.macro DICT_PUSH type, name
  SET_ABC \type, $0xDeadBeef, \name
  call dictPush
.endm

.macro TEST_GET_DICT_TYPE_GLOBAL_ADDR typeVal, type
  movl \typeVal, %eax
  call getDictTypeGlobalAddr
  ASSERT_REG_EQ %eax, $dict\type
.endm
.macro TEST_DICT_GLOBAL_GET typeVal, field, expectedAddr, expectedValue
  movl \typeVal, %eax
  call getDictTypeGlobalAddr
  DICT_GLOBAL_GET_ADDR \field, %eax
  ASSERT_REG_EQ %eax, \expectedAddr
  ASSERT_REG_EQ (%eax), \expectedValue
.endm
TESTCASE dictMacros
.set TEST_HEAP, heapMin
.equ TEST_DICT_CODE_MIN_RTIME, TEST_HEAP
.set TEST_HEAP, TEST_DICT_CODE_MIN_RTIME+DEFAULT_DICT_SECTION_SIZE
.equ TEST_DICT_CODE_MAX_RTIME, TEST_HEAP
  TEST_GET_DICT_TYPE_GLOBAL_ADDR $DICT_TYPE_RTIME, RTime
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, ROOT, $dictRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_MIN, $dictRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_MIN, $dictCodeMinRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_HERE, $dictCodeHereRTime, $heapMin
  TEST_DICT_GLOBAL_GET $DICT_TYPE_RTIME, CODE_MAX, $dictCodeMaxRTime, $TEST_DICT_CODE_MAX_RTIME
  # TODO: test others
  TEST_GET_DICT_TYPE_GLOBAL_ADDR $DICT_TYPE_CTIME, CTime
  # TODO: test compiletime
  ret

test_dictWord1CStr: .byte 9
  .ascii "TestWord1"
test_dictNameDneCStr: .byte 3
  .ascii "DNE"
.align 4
TESTCASE dict
  movl dictMetaMinRTime, %eax
  PUSHP %eax # cache where word will be stored
  movl dictMetaHereRTime, %ebx
  DICT_PUSH $DICT_TYPE_RTIME, $test_dictWord1CStr
  GET_P 0, %ebx
  call assertEaxEbxEq  # Got put at the start of meta
  movl %eax, %edi
  ASSERT_REG_EQ (%edi),   $0  # nextAddr=0
  ASSERT_REG_EQ 4(%edi),  $0  # xt=0
  ASSERT_REG_EQ 8(%edi),  $0  # info0=0
  addl $0xC, %edi # edi=nameCStr
  ASSERT_CSTR_NOCASE_EQ $test_dictWord1CStr, %edi
  movl $test_dictWord1CStr, %ecx
  call dictFindName
  POPP %ebx
  call assertEaxEbxEq
  movl $test_dictNameDneCStr, %ecx
  call dictFindName
  ASSERT_REG_EQ %eax, $0
  ret

##############################
# Main Entrypoint and test suite

testSuite:
  call assertClean

  call testcase_pureAsm
  call testcase_pstack

  call testcase_bcmp
  call testcase_beqNocase
  call testcase_bmove
  call testcase_align4
  call testcase_sll

  call testcase_findWhitespace
  call testcase_fillSourceCStr
  call testcase_flush_sourceCStr_to_wordCStr
  call testcase_word

  call testcase_pjwHashEax
  call testcase_dictMacros
  call testcase_dict

  call assertClean
  ret

setupHeap:
  // Sets up the heap, including the dictionary and other data structures.
  movl $heapMin, heap
  call setupDict
  ret

setupStack:
  movl $rstackMinRoot, rstackMin
  movl $rstackMaxRoot, rstackMax
  movl $pstackMinRoot, pstackMin
  movl $pstackMaxRoot, pstackMax
  movl rstackMax, %esp  # clear rstack
  movl pstackMax, %ebp  # clear pstack
  jmp afterSetupStack # Note: cannot `ret` as we've modified rstack

mainStartNtStr: .ascii "Starting Triforth...\n\0"
.align 4
.globl _start
_start:
  movl %esp, initialEsp # arguments and environment variables stored here
  jmp setupStack
afterSetupStack:
  call setupHeap
  PNT_NTSTR $mainStartNtStr
  call testSuite
  EXIT $0

################################################################################
#### DATA SECTION -- mutable memory must go here.                           ####
################################################################################
//###############
// Data layout
// |-rstack----|-pstack-|-"code"-------------------|-memory---...
//
// Code contains _only_ code (not the word dictionary). The word dictionary is
// a normal linked-list, except the name, flags and type is all kept inside of
// "memory" and so can be dropped once the "compilation" is complete.
.section .data

######
# Stacks
.equ RSTACK_SIZE, 0x2000
rstackExtraRoot: .space 0x4
rstackMinRoot: .space RSTACK_SIZE # return stack
rstackMaxRoot:
.equ PSTACK_SIZE, 0x100
pstackExtraRoot: .space 0x20      # guaranteed space above the pstack
pstackMinRoot: .space PSTACK_SIZE # data stack
pstackMaxRoot:
rstackMin: .int 0
rstackMax: .int 0
pstackMin: .int 0
pstackMax: .int 0

######
# Global Variables / State / Buffers
initialEsp: .int 0 # initial %esp register, use to get args/ENV
pntFd: .int STDERR # where to PNT to
.equ EMIT_BUFFER_SIZE, 16
sourceFd: .int STDIN   # where the source code is read from
.equ SOURCE_STR_CNT_MAX, 0x46  # maximim bytes of source read
.equ WORD_CNT_MAX, 0x23        # maximum word size
.equ WORD_STACK_SIZE, WORD_CNT_MAX+1
sourceEof: .byte 0 # Whether an EOF was encountered at the last read
sourceCStr: .byte 0 # Used for reading source word
sourceStr: .space SOURCE_STR_CNT_MAX
wordCStr: .byte 0
wordStr: .space WORD_CNT_MAX
errno: .int 0
panic: .int panicDefault # function to call when panicing
panicErr: .int 0  # error to print while panicing

.macro DECLARE_DICT type
  dict\type :
  dictCodeMin\type  : .int 0
  dictCodeHere\type : .int 0
  dictCodeMax\type  : .int 0
  dictMetaMin\type  : .int 0
  dictMetaHere\type : .int 0
  dictMetaMax\type  : .int 0
.endm
DECLARE_DICT RTime
DECLARE_DICT CTime
DECLARE_DICT LTime

dictBucketsAddr : .int 0
dictBucketsAndValue : .int 0 # Value used to MOD via AND (power_of_2-1)

######
# Debugging / Testing Variables
registerSnapshot: .space 0x20
testingNtStr: .int 0
panicRestore: .int 0
testSpace_00: .int 0
testSpace_04: .int 0
testSpace_08: .int 0
testSpace_0C: .int 0
testSpace_10: .int 0

heap: .int 0 # see setupMem
.align 0x100
heapMin:                     .space 0x200000  # 2MiB TODO: do dynamically from runtime
heapDefaultMax:
